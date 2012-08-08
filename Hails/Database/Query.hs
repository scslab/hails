{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable,
             FlexibleInstances,
             TypeSynonymInstances #-}

{- |

This module exports the basic types used to create queries and
selections. Different from standard MongoDB, Hails queries are limited
to 'SearchableField's (similarly, ordering a query result is limited
to such fields) and projections are disallowed. The later is a result
of allowing policy modules to express a labeling policy as a function
of a document -- hence we cannot determine at compile time if a field
is used in a policy and thus must be included in the projection. 

-}

module Hails.Database.Query (
  -- * Write
    InsertLike(..)
  -- * Read
  -- ** Selection
  , Select(..)
  , Selection(..)
  , Selector
  -- ** Query
  , Query(..)
  , QueryOption(..)
  , Limit
  , BatchSize
  , Order
  -- * Query failures
  , DBError(..)
  ) where


import           Data.Maybe (listToMaybe)
import           Data.Word (Word32)
import qualified Data.Set as Set
import           Data.Typeable

import           Control.Monad
import           Control.Exception (Exception)

import qualified Database.MongoDB as Mongo
import           Database.MongoDB.Query ( QueryOption(..)
                                        , Limit
                                        , BatchSize)

import           LIO
import           LIO.DCLabel
import           LIO.Labeled.TCB (unlabelTCB)

import           Hails.Data.Hson
import           Hails.Data.Hson.TCB
import           Hails.Database.Core
import           Hails.Database.TCB

--
-- Query
--


-- | Fields to sort by. Each one is associated with 1 or -1.
-- E.g. @[x '-:' 1, y '-:' -1]@ means sort by @x@ ascending
-- then @y@ descending.
type Order = BsonDocument

-- | Use select to create a basic query with defaults, then modify if
-- desired. Example: @(select sel col) {limit =: 10}@. For simplicity,
-- and since policies may be specified in terms of arbitrary fields,
-- Hails queries do not allow for projection specification.  The
-- 'selection' and 'sort' fields are restricted to 'SearchableField's, or
-- the @"_id"@ field that is implicitly a 'SearchableField'.
data Query = Query { options :: [QueryOption]
                   -- ^ Query options, default @[]@.
                   , selection :: Selection
                   -- ^ @WHERE@ clause,default @[]@.
                   -- Non-'SearchableField's ignored.
                   , skip :: Word32
                   -- ^ Number of documents to skip, default 0.
                   , limit :: Limit
                   -- ^ Max number of documents to return. Default, 0,
                   -- means no limit.
                   , sort :: Order
                   -- ^ Sort result by given order, default @[]@.
                   -- Non-'SearchableField's ignored.
                   , batchSize :: BatchSize
                   -- ^ The number of document to return in each
                   -- batch response from the server. 0 means
                   -- MongoDB default.
                   , hint :: Order
                   -- ^ Force mongoDB to use this index, default @[]@,
                   -- no hint.  
                   -- Non-'SearchableField's ignored.
                   }

-- | Filter for a query, analogous to the @WHERE@ clause in
-- SQL. @[]@ matches all documents in collection. For example,
-- @[x '-:' a, y '-:' b]@ is analogous to
-- @WHERE x = a AND y = b@ in SQL.
--
-- /Note/: only 'FieldName's of 'SearchableField's may be used in
-- selections, and thus all other fields are ignored.
type Selector = BsonDocument


-- | A @Section@ is a 'Selector' query on a 'Collection'. In other
-- words, a @Selection@ is the necessary information for performing a
-- database query.
data Selection = Selection { selectionSelector :: Selector
                           -- ^ Selection query.
                           , selectionCollection :: CollectionName
                           -- ^ Collection to perform query on.
                           } deriving (Show)

-- | Class used to simplicy the creation of a 'Selection'/'Query'.
-- Specifically, 'select' can be used to create a 'Section' in a
-- straight foward manner, but similarly can be used to create a
-- 'Query' with a set of default options.
class Select selectionOrQuery where
  -- | Given a selector and collection name create a 'Query'.
  -- The resultant type depends on the use case, for example,
  -- in 'find' @select mySel myCol@ is a 'Query', but in 'delete'
  -- it is a 'Selection'.
  select :: Selector -> CollectionName -> selectionOrQuery

instance Select Selection where
  select = Selection

instance Select Query where
  select s c = Query { options   = []
                     , selection = select s c
                     , skip      = 0
                     , limit     = 0
                     , sort      = []
                     , batchSize = 0
                     , hint      = [] 
                     }
--
-- Write
--

-- | Class used to generalize insertion and saving of documents.
-- Specifically, it permits reusing function names when inserting/saving
-- both already-labeled and unlabeled documents.
-- Minimal definition: 'insertP' and 'saveP'.
class InsertLike doc where
  -- | Insert document into collection and return its @_id@ value.  When
  -- performing an @insert@ it is required that the computation be able
  -- to write to both the database and collection. To this end, 'insert'
  -- internally applies 'guardWrite' on the database label and collection
  -- label. Of course, the computation must be able to name the
  -- collection in the database, and thus must be able to read the
  -- database collection map as verified by applying 'taint' to the
  -- collections label.
  -- 
  -- When inserting an unlabeled document, all policies must  be
  -- succesfully applied using 'applyCollectionPolicyP' and the document
  -- must be \"well-typed\" (see 'applyCollectionPolicyP').
  -- 
  -- When inserting an already-labeled document, the labels on fields
  -- and the document itself are compared against the policy-generated
  -- labels. Note that this approach allows an untrusted piece of code
  -- to insert a document it could not label according to the policy
  -- module.
  insert :: CollectionName
         -> doc
         -> DBAction HsonValue
  insert = insertP noPriv

  -- | Same as 'insert' except it does not return @_id@
  insert_ :: CollectionName
          -> doc
          -> DBAction ()
  insert_ c d = void $ insert c d

  -- | Same as 'insert', but uses privileges when applying the
  -- policies and performing label comparisons.
  insertP :: DCPriv
          -> CollectionName
          -> doc
          -> DBAction HsonValue

  -- | Same as 'insertP' except it does not return the @_id@.
  insertP_ :: DCPriv
           -> CollectionName
           -> doc
           -> DBAction ()
  insertP_ p c d = void $ insertP p c d

  -- | Update a document according to its @_id@ value. The IFC requirements
  -- subsume those of 'insert'. Specifically, in addition to being able
  -- to apply all the policies and requiring that the current label flow
  -- to the label of the collection and database, @save@ requires that 
  -- the current label flow to the label of the existing database
  -- record (i.e, the existing document can be overwritten).
  save :: CollectionName
       -> doc
       -> DBAction ()
  save = saveP noPriv

  -- | Same as 'save', but uses privileges when applying the
  -- policies and performing label comparisons.
  saveP :: DCPriv
        -> CollectionName
        -> doc
        -> DBAction ()

instance InsertLike HsonDocument where
  insertP priv cName doc = do
    withCollection priv True cName $ \col -> do
      -- Already checked that we can write to DB and collection,
      -- apply policies:
      ldoc <- applyCollectionPolicyP priv col doc
      -- No IFC violation, perform insert:
      let bsonDoc = hsonDocToDataBsonDocTCB . unlabelTCB $ ldoc
      _id `liftM` (execMongoActionTCB $ Mongo.insert cName bsonDoc)
    where _id i = let i'@(HsonValue _) = dataBsonValueToHsonValueTCB i
                  in i'
  saveP = undefined


--
-- DB failures
--


-- | Exceptions thrown by invalid database queries.
data DBError = UnknownCollection   -- ^ Collection does not exist
             | UnknownPolicyModule -- ^ Policy module not found
               deriving (Show, Typeable)

instance Exception DBError

--
-- Helpers
--

-- | Perform an action against a collection. The current label is
-- raised to the join of the current label, database label and
-- collections label before performing the action.
-- If the @isWrite@ flag, this action is taken as a database write
-- and 'guardWriteP' is applied to the database and collection labels.
withCollection :: DCPriv
               -> Bool
               -> CollectionName
               -> (Collection -> DBAction a)
               -> DBAction a
withCollection priv isWrite cName act = do
  db <- getDatabaseP priv
  -- If this is a write: check that we can write to database:
  when isWrite $ guardWriteP priv (databaseLabel db)
  -- Check that we can read collection names associated with DB:
  cs <- unlabelP priv $ databaseCollections db
  -- Lookup collection name in the collection set associated with DB:
  col <- maybe (throwLIO UnknownCollection) return $ getCol cs
  -- If this is a write: check that we can write to collection:
  when isWrite $ guardWriteP priv (colLabel col)
  -- Execute action on collection:
  act col
    where getCol = listToMaybe . Set.toList . Set.filter ((==cName) . colName)
