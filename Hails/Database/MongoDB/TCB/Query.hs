{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings #-}

module Hails.Database.MongoDB.TCB.Query ( insert, insert_
                                        , insertP, insertP_
                                        , save, saveP
                                        , deleteOne, deleteOneP
                                        -- * Finding objects
                                        , find, findP
                                        , findOne, findOneP
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
-- desired. Example: @(select sel col) {limit =: 10}@. Note that unlike
-- MongoDB's query functionality, our queries do not allow for
-- projections (since policies may need a field that is not projects).
-- Both the selection and sorting are restricted to searchable fields.
--
-- TODO: add snapshot.
data Query l = Query { options :: [M.QueryOption]
                     -- ^ Query options, default @[]@.
                     , selection :: Selection l
                     -- ^ @WHERE@ clause,default @[]@.
                     , skip :: Word32
                     -- ^ Number of documents to skip, default 0.
                     , limit :: M.Limit
                     -- ^ Max number of documents to return. Default, 0,
                     -- means no limit.
                     , sort :: Order l
                     -- ^ Sort result by given order, default @[]@.
                     , batchSize :: M.BatchSize
                     -- ^ The number of document to return in each
                     -- batch response from the server. 0 means
                     -- Mongo default.
                     , hint :: Order l
                     -- ^ Force mongoDB to use this index (must be
                     -- only searchable fields). Default @[]@, no hint.  
                     }

-- | Convert a 'Query' to the mongoDB equivalent. Note: keys that have 
-- the prefix 'hailsInternalKeyPrefix' are filtered out.
queryToMQuery :: (Serialize l, Label l) => Query l -> M.Query
queryToMQuery q = M.Query { M.options = options q
                          , M.selection = selectionToMSelection $ selection q
                          , M.project = []
                          , M.skip = skip q
                          , M.limit = limit q
                          , M.batchSize = batchSize q
                          , M.sort = toBsonDoc $ sort q
                          , M.snapshot = False
                          , M.hint = toBsonDoc $ hint q
                          }


-- | Filter for a query, analogous to the @WHERE@ clause in
-- SQL. @[]@ matches all documents in collection. @["x" =: a,
-- "y" =: b]@ is analogous to @WHERE x = a AND y = b@ in SQL.
--
-- /Note/: all labeld (including policy-labeled) values are removed
-- from the @Selector@.
type Selector l = Document l


-- | Selects documents in specified collection that match the selector.
data Selection l = Selection { selector :: Selector l -- ^ Selector
                             , coll :: CollectionName -- ^ Collection operaing
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
                     , sort = []
                     , batchSize = 0
                     , hint = [] 
                     }

-- | Fields to sort by. Each one is associated with a @1@ or @-1@. For
-- example @[ "x" =: 1, "y" =: -1]@ denotes sort by @x@ ascending then
-- @y@ descending. The sorts allowed in an order must be searchable
-- fields.
type Order l = Document l

--
-- Write
--

class (LabelState l p s, Serialize l) => Insert l p s doc where
  -- | Insert document into collection and return its @_id@ value,
  -- which is created automatically if not supplied. It is required that
  -- the current label flow to the label of the collection and database
  -- (and vice versa). Additionally, the document must be well-formed
  -- with respect to the collection policy. In other words, all the
  -- labeled values must be below the collection clearance and the
  -- policy be applied successfully.
  insert :: CollectionName
         -> doc
         -> Action l p s (Value l)
  insert = insertP noPrivs

  -- | Same as 'insert' except it does not return @_id@
  insert_ :: CollectionName
          -> doc
          -> Action l p s ()
  insert_ c d = void $ insert c d

  -- | Same as 'insert', but uses privileges when applying the
  -- collection policies, and doing label comparisons.
  insertP :: p
          -> CollectionName
          -> doc
          -> Action l p s (Value l)
  insertP p colName doc = do
    db <- getDatabase
    bsonDoc <- mkDocForInsertTCB p colName doc
    liftAction $ liftM BsonVal $ M.useDb (dbIntern db) $ M.insert colName bsonDoc

  -- | Same as 'insertP' except it does not return @_id@
  insertP_ :: p
           -> CollectionName
           -> doc
           -> Action l p s ()
  insertP_ p c d = void $ insertP p c d

  -- | Update a document based on the @_id@ value. The IFC requirements
  -- subsume those of 'insert'. Specifically, in addition to being able
  -- to apply all the policies and requiring that the current label flow
  -- to the label of the collection and database @save@ requires that 
  -- the current label flow to the label of the existing database record.
  save :: CollectionName
        -> doc
        -> Action l p s ()
  save = saveP noPrivs

  -- | Like 'save', but uses privileges when performing label
  -- comparisons.
  saveP :: p
         -> CollectionName
         -> doc
         -> Action l p s ()

  -- | Convert a 'Document' to a MongoDB @Document@, applying policies
  -- and checking that we can insert to DB and collection.
  -- Because the returned document is \"serialized\" document, this
  -- function must be part of the TCB.
  mkDocForInsertTCB :: p
                    -> CollectionName
                    -> doc
                    -> Action l p s M.Document



-- | Perform an 'LIO' action on a 'CollectionPolicy'
doForCollectionP :: (LabelState l p s, Serialize l)
                 => p
                 -> CollectionName
                 -> (p -> Database l
                       -> CollectionPolicy l -> LIO l p s a)
                 -> Action l p s a
doForCollectionP p' colName act = do
  db <- getDatabase
  liftLIO $ withCombinedPrivs p' $ \p -> do
    -- Check that we can read collection names associated with DB:
    colMap <- unlabelP p $ dbColPolicies db
    -- Lookup collection name in the collection map associated  with DB:
    col <- maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
    -- Get the collection clearance:
    act p db col




instance (LabelState l p s, Serialize l) => Insert l p s (Document l) where
  saveP p colName doc = do
    db <- getDatabase
    -- check that we can insert documetn as is:
    bsonDoc <- mkDocForInsertTCB p colName doc
    case M.look "_id" bsonDoc of
      Nothing -> dbAct db $ M.insert colName bsonDoc
      Just i -> do
        mdoc <- findOneP p $ select ["_id" := BsonVal i] colName
        -- If document exists make sure that we can overwrite the
        -- existing document:
        maybe (return ()) (lioWGuard . labelOf) mdoc
        dbAct db $ M.save colName bsonDoc
    where lioWGuard l = liftLIO $ withCombinedPrivs p $ \p' -> wguardP p' l
          dbAct db = void . liftAction . M.useDb (dbIntern db)

  mkDocForInsertTCB p' colName doc = do
    ldoc <- doForCollectionP p' colName $ \p _ col ->
      withClearance (colClear col) $ applyRawPolicyP p col doc
    mkDocForInsertTCB p' colName ldoc

instance (LabelState l p s, Serialize l, Insert l p s (Document l)) =>
         Insert l p s (Labeled l (Document l)) where
  saveP p colName ldoc = do
    db <- getDatabase
    -- check that we can insert documetn as is:
    bsonDoc <- mkDocForInsertTCB p colName ldoc
    case M.look "_id" bsonDoc of
      Nothing -> dbAct db $ M.insert colName bsonDoc
      Just i -> do
        mdoc <- findOneP p $ select ["_id" := BsonVal i] colName
        -- If document exists make sure that we can overwrite the
        -- existing document:
        maybe (return ()) (lioWGuard . labelOf) mdoc
        dbAct db $ M.save colName bsonDoc
    where lioWGuard l = liftLIO $ withCombinedPrivs p $ \p' ->
            unless (leqp p' (labelOf ldoc) l) $ throwIO LerrHigh
          dbAct db = void . liftAction . M.useDb (dbIntern db)

  mkDocForInsertTCB p' colName ldoc = 
    doForCollectionP p' colName $ \p db col -> do
      -- Check that we can write to database:
      wguardP p (dbLabel db)
      -- Check that we can write to collection:
      wguardP p (colLabel col)
      -- Document was labeled, policy was OK, remove label
      let udoc = unlabelTCB ldoc
      -- Apply policies (data should not be labeled with a label
      -- that is above the collection clearance):
      asIfLDoc <- applyRawPolicyTCB col udoc
      -- Check that label of the passed in @Document@ `canflowto`
      -- the label that would be generated by the policy.
      unless (leqp p (labelOf ldoc) (labelOf asIfLDoc)) $ throwIO LerrHigh
      -- Check that 'Labeled' values have labels below clearance:
      guardLabeledVals udoc $ colClear col
      -- Check that 'SearchableField's are not set to labeled
      -- values:
      guardSerachables udoc col
      -- Policies applied, labels are below clearance,
      -- searchables are Bson values and unlabeled, done:
      return $ toBsonDoc udoc
    where guardLabeledVals ds c = forM_ ds $ \(_ := v) ->
            case v of
              (LabeledVal lv) -> unless (labelOf lv `leq` c) $
                                   throwIO $ LerrClearance
              _               -> return ()
          --
          guardSerachables ds col =
            let srchbls = searchableFields . colPolicy $ col
            in forM_ ds $ \(k := v) ->
                 case v of
                   (BsonVal _) -> return ()
                   _           -> when (k `elem` srchbls) $
                                    throwIO InvalidSearchableType

-- | Returns true if the clause contains only searchable fields from
-- the collection policy
validateSearchableClause :: M.Document -> CollectionPolicy l -> Bool
validateSearchableClause doc policy = and (map isSearchable doc)
  where isSearchable ("_id" M.:= _) = True
        isSearchable (k M.:= _) = maybe False isSearchableField $ 
                                    lookup k fieldPolicies
        fieldPolicies = rawFieldPolicies . colPolicy $ policy

--
-- Read
--

-- | Fetch documents satisfying query. A labeled 'Cursor' is returned,
-- which can be used to retrieve the actual 'Document's. Current label
-- is raised to the join of the collection, database, and
-- ccollection-policy label.
find :: (Serialize l, LabelState l p s)
     => Query l -> Action l p s (Cursor l)
find = findP noPrivs


-- | Same as 'find', but uses privileges when raising the current
-- label
findP :: (Serialize l, LabelState l p s)
      => p -> Query l -> Action l p s (Cursor l)
findP p' q' = do
  db <- getDatabase
  let q       = queryToMQuery q'
      slct    = M.selection q
      colName = M.coll slct
  col <- liftLIO $  withCombinedPrivs p' $ \p -> do
    -- Check that we can read collection names associated with database:
    colMap <- unlabelP p $ dbColPolicies db
    -- Lookup collection name in the collection map associated  with DB:
    maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
  -- Check that we can read from the database and collection:
  liftLIO $ withCombinedPrivs p' $ \p -> do
    taintP p $ (colLabel col) `lub` (dbLabel db)
  -- Make sure that the selection, sort and hint soleley contain
  -- searchable fields:
  unless (and $ map (validate col) [ M.selector slct, M.sort q, M.hint q]) $
    liftIO $ throwIO InvalidFieldPolicyType
  -- Perform actual fetch:
  cur <- liftAction $ M.useDb (dbIntern db) $ M.find (q {M.project = []})
  -- Return a labeled cursor
  return $ Cursor { curLabel   = (colLabel col) `lub` (dbLabel db)
                  , curIntern  = cur 
                  , curProject = M.project q
                  , curPolicy  = col }
    where validate = flip validateSearchableClause

-- | Fetch the first document satisfying query, or @Nothing@ if not
-- documents matched the query.
findOne :: (LabelState l p s, Serialize l)
         => Query l -> Action l p s (Maybe (LabeledDocument l))
findOne = findOneP noPrivs

-- | Same as 'findOne', but uses privileges when performing label
-- comparisons.
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

-- | Same as 'next', but usess privileges raising the current label.
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
    Just d -> Just <$> (liftLIO $ applyProjection `liftM`
                                    applyRawPolicyTCB (curPolicy cur) d)
    where fromBsonDoc' = maybe Nothing fromBsonDocStrict
          applyProjection doc =
            if null $ curProject cur
              then doc
              else let udoc = unlabelTCB doc
                   in labelTCB (labelOf doc) $ filter inProjection udoc
          inProjection (k := _) = case M.look k $ curProject cur of
                                     Just (M.Int32 1) -> True
                                     _                -> False
--
-- Delete
--

-- | Given a query, delete first object in selection. In addition to
-- being able to read the object, write to the database and collection,
-- it must be that the current label flow to the label of the existing
-- document.
deleteOne :: (LabelState l p s, Serialize l)
          =>  Selection l -> Action l p s ()
deleteOne = deleteOneP noPrivs

-- | Same as 'deleteOne', but uses privileges when performing label
-- comparisons.
deleteOneP :: (LabelState l p s, Serialize l)
           => p -> Selection l -> Action l p s ()
deleteOneP p' sel = do
  p <- liftLIO $ withCombinedPrivs p' return
  let colName = coll sel
  mobj <- findOneP p $ select (selector sel) colName
  voidIfNothing mobj $ \ldoc -> do
    doForCollectionP p' (coll sel) $ \_ db col -> do
      -- Check that we can write to database:
      wguardP p (dbLabel db)
      -- Check that we can write to collection:
      wguardP p (colLabel col)
      -- Check that we can overwrite document:
      wguardP p $ labelOf ldoc
    i <- look "_id" $ unlabelTCB ldoc
    dbAct . M.deleteOne . selectionToMSelection $ select ["_id" := i] colName
   where dbAct act = do
          db <- getDatabase
          void . liftAction . M.useDb (dbIntern db) $ act
         voidIfNothing mv m = maybe (return ()) m mv

