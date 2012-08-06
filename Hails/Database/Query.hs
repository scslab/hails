{-# LANGUAGE Trustworthy #-}

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
  -- * Selection
    Selection(..) 
  , Selector
  -- * Query
  , Query(..)
  , QueryOption(..)
  , Limit
  , BatchSize
  , Order
  ) where


import           Data.Word (Word32)

import           Database.MongoDB.Query ( QueryOption(..)
                                        , Limit
                                        , BatchSize)

import           Hails.Data.Hson
import           Hails.Database.Core

--
-- Query
--


-- | Fields to sort by. Each one is associated with 1 or -1.
-- E.g. @["x" '-:' 1, "y" '-:' -1]@ means sort by @x@ ascending
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
-- @["x" '-:' a, "y" '-:' b]@ is analogous to @WHERE x = a AND y = b@
-- in SQL.
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
  select :: Selector -> CollectionName -> selectionOrQuery
  -- ^ Given a selector and collection name create a 'Query'.
  -- The resultant type depends on the use case, for example,
  -- in 'find' @select mySel myCol@ is a 'Query', but in 'delete'
  -- it is a 'Selection'.

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
