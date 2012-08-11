{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts,
             DeriveDataTypeable,
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
  -- * Applying policies
  , applyCollectionPolicyP
  -- ** Policy errors
  , PolicyError(..)
  -- * Internal
  , typeCheckDocument
  ) where


import           Data.Maybe
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word32)
import qualified Data.Set as Set
import           Data.Typeable
import qualified Data.Traversable as T

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

--
-- Policies
--


-- | Apply a collection policy the given document, using privileges
-- when labeling the document and performing label comparisons.
-- The labeling proceeds as follows:
--
-- * If two fields have the same 'FieldName', only the first is kept.
--   This filtering is only perfomed at the top level.
--
-- * Each policy labeled value ('HsonLabeled') is labled if the policy
--   has not been applied. If the value is already labeled, then the
--   label is checked to be equivalent to that generated by the policy.
--   In both cases a failure results in 'PolicyViolation' being thrown;
--   the actual error must be hidden to retain the opaqueness of
--   'PolicyLabeled'.
--
--   /Note:/ For each 'FieldNamed' in the policy there /must/ be a
--   field in the document corresponding to it. Moreover its \"type\"
--   must be correct: all policy labeled values must be 'HsonLabeled'
--   values and all searchable fields must be 'HsonValue's. The @_id@
--   field is always treated as a 'SearchableField'.
--
-- * The resulting document (from the above step) is labeled according
--   to the collection policy.
--
-- The labels on 'PolicyLabeled' values and the document must be bounded
-- by the current label and clearance as imposed by 'guardAllocP'.
-- Additionally, these labels must flow to the label of the collection
-- clearance. (Of course, in both cases privileges are used to allow for
-- more permissive flows.)
applyCollectionPolicyP :: MonadDC m
                       => DCPriv        -- ^ Privileges
                       -> Collection    -- ^ Collection and policies
                       -> HsonDocument  -- ^ Document to apply policies to
                       -> m (LabeledHsonDocument)
applyCollectionPolicyP p col doc0 = liftLIO $ do
  let doc1 = List.nubBy (\f1 f2 -> fieldName f1 == fieldName f2) doc0
  typeCheckDocument fieldPolicies doc1
  withClearance (colClearance col) $ do
    -- Apply fied policies:
    doc2 <- T.for doc1 $ \f@(HsonField n v) ->
      case v of
        (HsonValue _) -> return f
        (HsonLabeled pl) -> do
          -- NOTE: typeCheckDocument MUST be run before this:
          let (FieldPolicy fieldPolicy) = fieldPolicies Map.! n
              l = fieldPolicy doc1
          case pl of
            (NeedPolicyTCB bv) -> do
              lbv <- labelP p l bv `onException` throwLIO PolicyViolation
              return (n -: hasPolicy lbv)
            (HasPolicyTCB lbv) -> do 
              unless (labelOf lbv == l) $ throwLIO PolicyViolation
              return f
    -- Apply document policy:
    labelP p (docPolicy doc2) doc2
  where docPolicy     = documentLabelPolicy . colPolicy $ col
        fieldPolicies = fieldLabelPolicies  . colPolicy $ col

-- | This function \"type-checks\" a document against a set of policies.
-- Specifically, it checks that the set of policy labeled values is the
-- same between the policy and document, and searchable fields are not
-- policy labeled.
typeCheckDocument :: Map FieldName FieldPolicy -> HsonDocument -> DC ()
typeCheckDocument ps doc = do
  -- Check that every policy-named value is well-typed
  void $ T.for psList $ \(k,v) -> do
    let mv' = look k doc
        v' = fromJust mv'
    unless (isJust mv') $ throwLIO $ TypeError $ 
      "Missing field with name " ++ show k
    case v of
      SearchableField -> isHsonValue   k v'
      FieldPolicy _   -> isHsonLabeled k v'
  -- Check that no policy-labeled values not named in the policy
  -- exist:
  let doc' = exclude (map fst psList) doc
  unless (isBsonDoc doc') $ throwLIO $ TypeError $
     "Fields " ++ show (map fieldName doc') ++ " should NOT be policy labeled."
        where psList = Map.toList ps
              isHsonValue _ (HsonValue _) = return ()
              isHsonValue k _ = throwLIO $ TypeError $
                show k ++ " should NOT be policy labeled"
              isHsonLabeled _ (HsonLabeled _) = return ()
              isHsonLabeled k _ = throwLIO $ TypeError $
                show k ++ " should be policy labeled"


--
-- Policy error
--

-- | A document policy error.
data PolicyError = TypeError String -- ^ Document is not \"well-typed\"
                 | PolicyViolation  -- ^ Policy has been violated
                 deriving (Show, Typeable)

instance Exception PolicyError
