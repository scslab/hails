{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts,
             DeriveDataTypeable,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeSynonymInstances #-}

{- |

This module exports the basic types used to create queries and
selections. Different from standard MongoDB, Hails queries are limited
to 'SearchableField's (similarly, ordering a query result is limited
to such fields) and projections are carried out by this library and
not the database. The later is a result of allowing policy modules to
express a labeling policy as a function of a document -- hence we
cannot determine at compile time if a field is used in a policy and
thus must be included in the projection. 

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
  , Order(..), orderName
  -- * Find
  , Cursor, curLabel
  , find, findP
  , next, nextP
  , findOne, findOneP
  -- * Query failures
  , DBError(..)
  -- * Applying policies
  , applyCollectionPolicyP
  -- ** Policy errors
  , PolicyError(..)
  -- * Internal
  , typeCheckDocument
  ) where


import           Prelude hiding (lookup)
import           Data.Maybe
import           Data.List (sortBy)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word32)
import qualified Data.Set as Set
import           Data.Typeable
import qualified Data.Text as Text
import qualified Data.Traversable as T

import           Control.Monad
import           Control.Exception (Exception)

import qualified Data.Bson        as Bson
import qualified Database.MongoDB as Mongo
import           Database.MongoDB.Query ( QueryOption(..)
                                        , Limit
                                        , BatchSize)

import           LIO
import           LIO.DCLabel
import           LIO.Labeled.TCB (unlabelTCB, labelTCB)

import           Hails.Data.Hson
import           Hails.Data.Hson.TCB
import           Hails.Database.Core
import           Hails.Database.TCB
import           Hails.Database.Query.TCB

--
-- Query
--


-- | Sorting fields in 'Asc'ending or 'Desc'ending order.
data Order = Asc FieldName  -- ^ Ascending order
           | Desc FieldName -- ^ Descending order
           deriving (Eq, Ord, Show)

-- | Get the field name in the order.
orderName :: Order -> FieldName
orderName (Asc n) = n
orderName (Desc n) = n

-- | Use select to create a basic query with defaults, then modify if
-- desired. Example: @(select sel col) {limit =: 10}@. For simplicity,
-- and since policies may be specified in terms of arbitrary fields,
-- The 'selection' and 'sort' fields are restricted to 'SearchableField's,
-- or the @"_id"@ field that is implicitly a 'SearchableField'.
data Query = Query { options :: [QueryOption]
                   -- ^ Query options, default @[]@.
                   , selection :: Selection
                   -- ^ @WHERE@ clause,default @[]@.
                   -- Non-'SearchableField's ignored.
                   , project :: [FieldName]
                   -- ^ The fields to project. Default @[]@
                   -- corresponds to all.
                   , skip :: Word32
                   -- ^ Number of documents to skip, default 0.
                   , limit :: Limit
                   -- ^ Max number of documents to return. Default, 0,
                   -- means no limit.
                   , sort :: [Order]
                   -- ^ Sort result by given order, default @[]@.
                   -- Non-'SearchableField's ignored.
                   , batchSize :: BatchSize
                   -- ^ The number of document to return in each
                   -- batch response from the server. 0 means
                   -- MongoDB default.
                   , hint :: [FieldName]
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
                     , project   = []
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
         -> DBAction ObjectId
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
          -> DBAction ObjectId

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
  -- Note that a find is performed if the provided document contains
  -- an @_id@ field. This lookup does _not_ leak timing information
  -- since the @_id@ field is always searchable and thus solely
  -- protected by the collection label (which the computation is
  -- tainted by).
  saveP :: DCPriv
        -> CollectionName
        -> doc
        -> DBAction ()

instance InsertLike HsonDocument where
  insertP priv cName doc = do
    withCollection priv True cName $ \col -> do
      -- Already checked that we can write to DB and collection,
      -- apply policies:
      ldoc <- liftLIO $ applyCollectionPolicyP priv col doc
      -- No IFC violation, perform insert:
      let bsonDoc = hsonDocToDataBsonDocTCB . unlabelTCB $ ldoc
      _id `liftM` (execMongoActionTCB $ Mongo.insert cName bsonDoc)
    where _id i = let HsonValue (BsonObjId i') = dataBsonValueToHsonValueTCB i
                  in i'
  saveP priv cName doc = do
    withCollection priv True cName $ \col -> do
      -- Already checked that we can write to DB and collection,
      -- apply policies:
      ldoc <- liftLIO $ applyCollectionPolicyP priv col doc
      let _id_n = Text.pack "_id"
      case lookup _id_n doc of
        Nothing -> saveIt ldoc
        Just (_id :: ObjectId) -> do
          mdoc <- findOneP priv $ select [_id_n -: _id] cName
          -- If document exists, check that we can overwrite it:
          maybe (return ()) (guardWriteP priv . labelOf) mdoc
          -- Okay, save document:
          saveIt ldoc
      where saveIt ldoc =
              let bsonDoc = hsonDocToDataBsonDocTCB . unlabelTCB $ ldoc
              in execMongoActionTCB $ Mongo.save cName bsonDoc

instance InsertLike LabeledHsonDocument where
  -- | When inserting a labeled document, all the policy-labeled
  -- fields  must already be labeled with the correct label.
  -- Additinally, the document label must flow to the label of the
  -- policy-specified document label. Note, however, that that the
  -- current computation may insert a document it could otherwise not
  -- have created.
  insertP priv cName ldoc' = do
    guardInsertOrSaveLabeledHsonDocument priv cName ldoc' $ \ldoc ->
      -- No IFC violation, perform insert:
      let bsonDoc = hsonDocToDataBsonDocTCB . unlabelTCB $ ldoc
      in _id `liftM` (execMongoActionTCB $ Mongo.insert cName bsonDoc)
    where _id i = let HsonValue (BsonObjId i') = dataBsonValueToHsonValueTCB i
                  in i'

  -- | When saving a labeled document, all the policy-labeled
  -- fields  must already be labeled with the correct label.
  -- Additinally, the document label must flow to the label of the
  -- policy-specified document label and existing document.
  -- Note, however, that that the current computation may save a
  -- document it could otherwise not have created.
  saveP priv cName ldoc' = do
    guardInsertOrSaveLabeledHsonDocument priv cName ldoc' $ \ldoc ->
      let doc   = unlabelTCB ldoc
          _id_n = Text.pack "_id"
      in case lookup _id_n doc of
        Nothing -> saveIt ldoc
        Just (_id :: ObjectId) -> do
          mdoc <- findOneP priv $ select [_id_n -: _id] cName
          -- If document exists, check that we can overwrite it:
          maybe (return ()) (guardWriteP' (labelOf ldoc) . labelOf) mdoc
          -- Okay, save document:
          saveIt ldoc
     where guardWriteP' lnew lold = 
             unless (canFlowToP priv lnew lold) $ throwLIO $ 
               VMonitorFailure {
                 monitorFailure = CanFlowToViolation
               , monitorMessage = "New document label doesn't flow to the old" }
           saveIt ldoc =
             let bsonDoc = hsonDocToDataBsonDocTCB . unlabelTCB $ ldoc
             in execMongoActionTCB $ Mongo.save cName bsonDoc

--
-- Helper
--

-- | Save or insert document. This function is used to check that:
--
-- 1. The current computation can write to the database and collection.
--
-- 2. The labeled document is properly labeled: all policy-labeled
--    fields have the label as if generated by the policy, the
--    document label flows to the policy-generated label, and the
--    document is well-typed (i.e., searchables are not policy
--    labeled, etc.). Moreover all labels are checked to be below the
--    collection clearance by 'withColletion'.
--
-- After the check the supplied function is applied to the
-- policy-labeled document (which should be the same as the supplied
-- document, except for possibly the document label.)
guardInsertOrSaveLabeledHsonDocument
  :: DCPriv              -- ^ Privileges
  -> CollectionName      -- ^ Collection to insert/save to
  -> LabeledHsonDocument -- ^ Original documentk
  -> (LabeledHsonDocument -> DBAction a) -- ^ Insert/save action
  -> DBAction a
guardInsertOrSaveLabeledHsonDocument priv cName ldoc act = do
    withCollection priv True cName $ \col -> do
      -- Already checked that we can write to DB and collection
      -- Document is labeled, remove label:
      let doc = unlabelTCB ldoc
      -- Apply policies to the unlabeled document,
      -- asserts that labeled values are below collection clearance:
      dbPriv <- dbActionPriv `liftM` getActionStateTCB
      {- DON'T HIDE EXCEPTION:
      ldocTCB <- liftLIO $ onExceptionP priv 
        (applyCollectionPolicyP dbPriv col doc)
        (throwLIO PolicyViolation)
      -}
      ldocTCB <- liftLIO $ applyCollectionPolicyP dbPriv col doc
      -- Check that all the fields are the same (i.e., if there was a
      -- unlabeled PolicyLabeled value an this will fail):
      let same = compareDoc doc  (unlabelTCB ldocTCB)
      unless same $ throwLIO PolicyViolation
      -- Check that label of the passed in document `canFlowToP`
      -- the label of document created by the policy:
      unless (canFlowToP priv (labelOf ldoc) (labelOf ldocTCB)) $
        throwLIO PolicyViolation
      -- Perform action on policy-labeled document:
      act ldocTCB
  where compareDoc d1' d2' = 
          let d1 = sortDoc d1'
              d2 = sortDoc d2'
          in map fieldName d1 == map fieldName d2 
          && (and $ zipWith compareField d1 d2)
        compareField (HsonField n1 (HsonValue v1))
                     (HsonField n2 (HsonValue v2)) =
                       n1 == n2 && v1 == v2
        compareField (HsonField n1 (HsonLabeled (HasPolicyTCB v1)))
                     (HsonField n2 (HsonLabeled (HasPolicyTCB v2))) =
                       n1 == n2 && labelOf v1 == labelOf v2
        compareField _ _ = False
        sortDoc = sortBy (\f1 f2 -> fieldName f1 `compare` fieldName f2)

--
-- Read
--

-- | Fetch documents satisfying query. A labeled 'Cursor' is returned,
-- which can be used to retrieve the actual 'HsonDocument's.  For this
-- function to succeed the current computation must be able to read from
-- the database and collection (implicilty the database's
-- collection-set). This is satisfied by applying 'taint' to the join
-- join of the collection, database, and ccollection-set label.
-- The curor label is labeled by the 'upperBound' of the database and
-- collection labels and must be used within the same 'withPolicyModule'
-- block.
--
-- Note that this function is quite permissive in the queries it
-- accepts. Specifically, any non-'SearchableField's used in 'sort',
-- 'order', or 'hint' are /ignored/ (as opposed to throwing an
-- exception).
find :: Query -> DBAction Cursor
find = findP noPriv

-- | Same as 'find', but uses privileges when reading from the
-- collection and database.
findP :: DCPriv -> Query -> DBAction Cursor
findP priv query = do
  let cName = selectionCollection . selection $ query
  dbLabel <- (databaseLabel . dbActionDB) `liftM` getActionStateTCB
  withCollection priv False cName $ \col -> do
      -- Already checked that we can read from DB and collection.
    let policy = colPolicy col
        -- Get all the searchable fields:
        searchables = Map.keys . Map.filter isSearchable $
                              fieldLabelPolicies policy
        -- Remove any non-'SearchableField's from the hint
        hint' = hint query `List.intersect` searchables
        -- Remove any non-'SearchableField's from the sorthint
        sort' = filter (\f -> orderName f `elem` searchables) $ sort query
        -- Remove any non-'SearchableField's from the selection
        sel = selection $ query
        selector' = include searchables $ selectionSelector sel
        selection' = sel { selectionSelector = selector' }
        -- Create the new /clean/ query:
        query' = query { sort = sort', hint = hint', selection = selection' }
        -- Convert the query to Mongo's query type:
        mongoQuery = queryToMongoQueryTCB query'
    cur <- execMongoActionTCB $ Mongo.find mongoQuery
    return $ CursorTCB { curLabel      = colLabel col `lub` dbLabel
                       , curInternal   = cur
                       , curProject    = project query'
                       , curCollection = col }
      where isSearchable SearchableField = True
            isSearchable _ = False

-- | Return next 'HsonDocument' in the query result, or 'Nothing' if
-- finished.  Note that the current computation must be able to read from
-- the labeled 'Cursor'. To enforce this, @next@ uses 'taint' to raise
-- the current label to join of the current label and 'Cursor'\'s label.
-- The returned document is labeled according to the underlying
-- 'Collection' policy.
next :: Cursor -> DBAction (Maybe LabeledHsonDocument)
next = nextP noPriv

-- | Same as 'next', but usess privileges when raising the current label.
nextP :: DCPriv -> Cursor -> DBAction (Maybe LabeledHsonDocument)
nextP p cur = do
  -- Raise current label, can read from DB+collection:
  taintP p $ curLabel cur
  -- Read the document:
  mMongoDoc <- execMongoActionTCB $ Mongo.next $ curInternal cur
  case mMongoDoc of
    Nothing -> return Nothing
    Just mongoDoc -> do
      let doc0 = dataBsonDocToHsonDocTCB mongoDoc
      dbPriv <- dbActionPriv `liftM` getActionStateTCB
      ldoc <- liftLIO $ do 
                applyCollectionPolicyP dbPriv (curCollection cur) doc0
      let doc = unlabelTCB ldoc
          l   = labelOf ldoc
          proj = case curProject cur of
                  [] -> id
                  xs -> include xs
      return . Just . labelTCB l . proj $ doc

-- | Fetch the first document satisfying query, or 'Nothing' if not
-- documents matched the query.
findOne :: Query -> DBAction (Maybe LabeledHsonDocument)
findOne = findOneP noPriv

-- | Same as 'findOne', but uses privileges when performing label
-- comparisons.
findOneP :: DCPriv -> Query -> DBAction (Maybe LabeledHsonDocument)
findOneP p q = findP p q >>= nextP p

--
-- Helpers
-- 

-- | Convert a query to queries used by "Database.Mongo"
queryToMongoQueryTCB :: Query -> Mongo.Query
queryToMongoQueryTCB q = Mongo.Query {
    Mongo.options   = options q
  , Mongo.selection = selectionToMongoSelectionTCB $ selection q
  , Mongo.project   = []
  , Mongo.skip      = skip q
  , Mongo.limit     = limit q
  , Mongo.sort      = map orderToField $ sort q
  , Mongo.snapshot  = False
  , Mongo.batchSize = batchSize q
  , Mongo.hint      = map (\f -> (Bson.=:) f (1::Int)) $ hint q
  } where orderToField (Asc n)  = (Bson.=:) n (1::Int)
          orderToField (Desc n) = (Bson.=:) n (-1::Int)

-- | Convert a selection to selection used by "Database.Mongo"
selectionToMongoSelectionTCB :: Selection -> Mongo.Selection
selectionToMongoSelectionTCB s = Mongo.Select {
    Mongo.selector = bsonDocToDataBsonDocTCB $ selectionSelector s
  , Mongo.coll     = selectionCollection s }


--
-- Helpers
--

-- | Perform an action against a collection. The current label is
-- raised to the join of the current label, database label and
-- collection-set label before performing the action.
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
applyCollectionPolicyP :: DCPriv        -- ^ Privileges
                       -> Collection    -- ^ Collection and policies
                       -> HsonDocument  -- ^ Document to apply policies to
                       -> DC (LabeledHsonDocument)
applyCollectionPolicyP p col doc0 = do
  let doc1 = List.nubBy (\f1 f2 -> fieldName f1 == fieldName f2) doc0
  typeCheckDocument fieldPolicies doc1
  c <- getClearance
  withClearanceP p ((colClearance col) `lowerBound` c) $ do
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
-- same between the policy and document, searchable fields are not
-- policy labeled, and all searchable/policy-labeled fields named in
-- the collection policy are present in the document (except for @_id@).
typeCheckDocument :: Map FieldName FieldPolicy -> HsonDocument -> DC ()
typeCheckDocument ps doc = do
  -- Check that every policy-named value exists and is well-typed
  void $ T.for psList $ \(k,v) -> do
    case look k doc of
      -- Field exists in document, type check it:
      Just v' -> case v of
                   SearchableField -> isHsonValue   k v'
                   FieldPolicy _   -> isHsonLabeled k v'
      -- Ignore case where _id does not exist:
      _ | k == Text.pack "_id" -> return ()
      -- Field missing from document:
      _  -> throwLIO $ TypeError $ "Missing field with name " ++ show k
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
