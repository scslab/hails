{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Hails.Database.MongoDB.TCB.Query ( Insert(..)
                                        , findP
                                        , findOneP
                                        , next, nextP
                                        , Query(..), Selection(..), Selector
                                        , select
                                        ) where

import Hails.Database.MongoDB.TCB.Access
import Hails.Database.MongoDB.TCB.Types

import LIO
import LIO.TCB

import Data.Word
import Data.Functor ((<$>))
import Data.Serialize (Serialize)
import qualified Data.Map as Map
import Hails.Data.LBson.TCB hiding (lookup)
import qualified Database.MongoDB as M

import Control.Monad.Reader hiding (liftIO)

-- | Use select to create a basic query with defaults, then modify if
-- desired. Example: @(select sel col) {limit = 10}@. Note that unlike
-- MongoDB's query functionality, our queries do not allow for
-- projections (since policies may need a field that is not projects).
--
-- TODO: add snapshot, hints, sorts (with correct tainting), etc.
data Query l = Query { options :: [M.QueryOption]
                     , selection :: Selection l
                     , skip :: Word32
                     -- ^ Number of documents to skip, default 0.
                     , limit :: M.Limit
                     -- ^ Max number of documents to return. Default, 0,
                     -- means no limit.
                     , batchSize :: M.BatchSize
                     -- ^ The number of document to return in each
                     -- batch response from the server. 0 means
                     -- Mongo default.
                     }

-- | Convert a 'Query' to the mongoDB equivalent.
queryToMQuery :: (Serialize l, Label l) => Query l -> M.Query
queryToMQuery q = M.Query { M.options = options q
                          , M.selection = selectionToMSelection $ selection q
                          , M.project = []
                          , M.skip = skip q
                          , M.limit = limit q
                          , M.batchSize = batchSize q
                          -- Not yet handled:
                          , M.sort = []
                          , M.snapshot = False
                          , M.hint = []
                          }


-- | A simple query is a 'Query' that retries the whole collection.
-- In other words, with a simple query you cannot specify a predicate
-- (@WHERE@ clause).
newtype SimpleQuery l = SimpleQuery (Query l)

-- | Filter for a query, analogous to the @WHERE@ clause in
-- SQL. @[]@ matches all documents in collection. @["x" =: a,
-- "y" =: b]@ is analogous to @WHERE x = a AND y = b@ in SQL.
--
-- /Note/: all labeld (including policy-labeled) values are removed
-- from the @Selector@.
--
-- TODO: allow queries on labeled values.
type Selector l = Document l

-- | Selects documents in specified collection that match the selector.
data Selection l = Selection { selector :: Selector l -- ^ Selector
                             , coll :: CollectionName -- ^ Collection operaing on
                             }

-- | Convert a 'Selection' to the mongoDB equivalent.
selectionToMSelection :: (Serialize l, Label l) => Selection l -> M.Selection
selectionToMSelection s = M.Select { M.selector = toBsonDoc $ selector s
                                   , M.coll = coll s }

-- | Convert a 'Selector' to a 'Selection' or 'Query'
class Select selectionOrQuery where
  select :: Label l => Selector l -> CollectionName -> selectionOrQuery l
  -- ^ 'Query' or 'Selection' that selects documents in collection that match
  -- selector. The choice of end type depends on use, for example, in 'find'
  -- @select sel col@ is a 'Query', but in delete it is a 'Selection'.

instance Select Selection where
  select = Selection

instance Select Query where
  select s c = Query { options = []
                     , selection = select s c
                     , skip = 0
                     , limit = 0
                     , batchSize = 0 }

instance Select SimpleQuery where
  select _ c = SimpleQuery $ select [] c

--
-- Write
--

-- | Class used to overload inserting labeled and unlabeled documents
-- into a collection. Only the definition for @inserP@ is needed.
class Label l => Insert l doc where
  -- | Insert document into collection and return its @_id@ value,
  -- which is created automatically if not supplied.
  insert :: (LabelState l p s, Serialize l)
         => CollectionName
         -> doc
         -> Action l p s M.Value
  insert = insertP noPrivs
  -- | Same as 'insert' except it does not return @_id@
  insert_ :: (LabelState l p s, Serialize l)
          => CollectionName
          -> doc
          -> Action l p s ()
  insert_ c d = insert c d >> return ()
  -- | Same as 'insert', but uses privileges when applying the
  -- collection policies, and doing label comparisons.
  insertP :: (LabelState l p s, Serialize l)
          => p 
          -> CollectionName
          -> doc
          -> Action l p s M.Value
  insertP p colName doc = do
    db <- getDatabase
    bsonDoc <- convertDocP p colName doc
    liftAction $ M.useDb (dbIntern db) $ M.insert colName bsonDoc
  -- | Same as 'insertP' except it does not return @_id@
  insertP_ :: (LabelState l p s, Serialize l)
           => p 
           -> CollectionName
           -> doc
           -> Action l p s ()
  insertP_ p c d = insertP p c d >> return ()
  -- | Update a document based on the @_id@ value. Checks that the
  -- current label can flow to the existing database record.
  saveP :: (LabelState l p s, Serialize l)
         => p
         -> CollectionName
         -> doc
         -> Action l p s ()
  saveP p colName doc = do
    db <- getDatabase
    bsonDoc <- convertDocP p colName doc
    case M.look "_id" bsonDoc of
      Nothing -> insertP_ p colName doc
      Just i -> do
        exist <- findOneP p (select ["_id" := BsonVal i] colName)
        case exist of
          Just rec -> liftLIO $ wguardP p $ labelOf rec
          Nothing -> return ()
        liftAction $ M.useDb (dbIntern db) $ M.save colName bsonDoc
  
  convertDocP :: (LabelState l p s, Serialize l)
              => p
              -> CollectionName
              -> doc
              -> Action l p s M.Document

instance Label l => Insert l (Document l) where
  convertDocP p' colName doc = do
      db <- getDatabase
      col <- liftLIO $  withCombinedPrivs p' $ \p -> do
        -- Check that we can read collection names associated with database:
        colMap <- unlabelP p $ dbColPolicies db
        maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
      let clearance = colClear col
      ldoc <- liftLIO $ withCombinedPrivs p' $ \p -> do
                -- Check that we can write to database:
                wguardP p (dbLabel db)
                -- Check that we can write to collection:
                wguardP p (colLabel col)
                -- Apply policies (data should not be labeled with a label
                -- that is above the collection clearance):
                ldoc <- withClearance clearance $ applyRawPolicyP p col doc
                -- Check that 'Labeled' values have labels below clearnce:
                guardLabeledVals (unlabelTCB ldoc) clearance
                -- Check that SearchableFields are not set to labeled
                -- values:
                let srchbls = searchableFields . colPolicy $ col
                forM_ (unlabelTCB ldoc)$ \(k := v) -> case v of
                  (BsonVal _) -> return ()
                  _ -> when (k `elem` srchbls) $ throwIO InvalidSearchableType
                -- Policies applied & labels are below clearance:
                return ldoc
      return $ toBsonDoc . unlabelTCB $ ldoc
        where guardLabeledVals []            _ = return ()
              guardLabeledVals ((_ := v):ds) c = do
                case v of
                  (LabeledVal lv) -> unless (labelOf lv `leq` c) $
                                       throwIO $ LerrClearance
                  _               -> return ()
                guardLabeledVals ds c

-- | Returns true if the clause contains only searchable fields from
-- the collection policy
validateSearchableClause :: M.Document -> CollectionPolicy l -> Bool
validateSearchableClause doc policy = and (map isSearchable cleanDoc)
  where isSearchable ("_id" M.:= _) = True
        isSearchable (k M.:= _) =
          case lookup k fieldPolicies of
            Just SearchableField -> True
            _ -> False
        fieldPolicies = rawFieldPolicies . colPolicy $ policy
        cleanDoc = map (\(f M.:= v) ->
                          f M.:= sanitizeBsonValue v) doc

--
-- Read
--

-- | Fetch documents satisfying query. A labeled 'Cursor' is returned,
-- which can be used to retrieve the actual 'Document's.
findP :: (Serialize l, LabelState l p s)
    => p -> Query l -> Action l p s (Cursor l)
findP p' q' = do
  db <- getDatabase
  let q = queryToMQuery q'
  let slct = M.selection q
  let colName = M.coll slct
  col <- liftLIO $  withCombinedPrivs p' $ \p -> do
    -- Check that we can read collection names associated with database:
    colMap <- unlabelP p $ dbColPolicies db
    maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
  unless ((validateSearchableClause (M.selector slct) col) &&
          (validateSearchableClause (M.sort q) col) &&
          (validateSearchableClause (M.hint q) col)) $
            liftIO $ throwIO InvalidFieldPolicyType
  cur <- liftAction $ M.useDb (dbIntern db) $ M.find (q {M.project = []})
  return $ Cursor { curLabel  = (colLabel col) `lub` (dbLabel db)
                  , curIntern = cur 
                  , curProject = M.project q
                  , curPolicy = col }

-- | Fetch the first document satisfying query, or @Nothing@ if not
-- documents matched the query.
findOneP :: (LabelState l p s, Serialize l)
         => p -> Query l -> Action l p s (Maybe (LabeledDocument l))
findOneP p q = findP p q >>= nextP p

-- | Return next document in query result, or @Nothing@ if finished.
-- The current label is raised to join of the current label and
-- 'Cursor' label. The document is labeled according to the
-- underlying 'Collection'\'s policies.
next :: (LabelState l p s, Serialize l)
     => Cursor l
     -> Action l p s (Maybe (LabeledDocument l))
next = nextP noPrivs

-- | Same as 'simpleNext', but usess privileges raising the current label.
nextP :: (LabelState l p s, Serialize l)
      => p
      -> Cursor l
      -> Action l p s (Maybe (LabeledDocument l))
nextP p' cur = do
  -- Rause current label, can read from DB+collection:
  liftLIO $ withCombinedPrivs p' $ \p -> taintP p (curLabel cur)
  md <- fromBsonDoc' <$> (liftAction $ M.next (curIntern cur))
  case md of
    Nothing -> return Nothing
    Just d -> Just <$> (liftLIO $ (fmap) applyProjection
                                $ applyRawPolicyTCB (curPolicy cur) d)
    where fromBsonDoc' = maybe Nothing fromBsonDocStrict
          applyProjection doc = if (length $ curProject cur) == 0
            then doc
            else
              let unsafeDoc = unlabelTCB doc
              in labelTCB (labelOf doc) $
                filter inProjection unsafeDoc
          inProjection (k := _) = case M.look k $ curProject cur of
            Just (M.Int32 1) -> True
            _ -> False

