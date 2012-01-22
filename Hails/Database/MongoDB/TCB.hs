{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO: remove:
{-# LANGUAGE OverloadedStrings #-}
--
module Hails.Database.MongoDB.TCB where

import LIO
import LIO.TCB ( LIO(..)
               , LabeledException(..)
               , LIOstate
               , unlabelTCB
               , labelTCB
               , rtioTCB
               , getTCB
               , putTCB
               , setLabelTCB
               , lowerClrTCB
               )
import LIO.MonadCatch
import Hails.Data.LBson.TCB

import Data.Typeable
import qualified Data.List as List

import Data.Word (Word32)
import Data.Maybe
import Data.Functor ((<$>))
import Data.Serialize (Serialize, encode, decode)
import Data.CompactString.UTF8 (append, isPrefixOf)

import Database.MongoDB.Connection
import Database.MongoDB ( Failure(..)
                        , AccessMode(..)
                        , QueryOption(..)
                        , Limit
                        , BatchSize
                        )
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.State (StateT)
import qualified Database.MongoDB as M

import qualified Control.Exception as E
import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.IO.Class as IO


-- ## REMOVE ########################################################

import LIO.DCLabel
import DCLabel.PrettyShow
import LIO.TCB (ioTCB)


main :: IO ()
main =  do
  pipe <- runIOE $ connect(host "127.0.0.1")
  (res,l) <- evalDC $ do
    let n = "noway" :: String
        p = "p455w0rd" :: String
        db = Database lbot "baseball" :: Database DCLabel
        c = Collection { colLabel = newDC ("sweet" :: String) (<>)
                       , colClear = ltop
                       , colIntern = "auth"
                       , colPolicy = RawPolicy {
                           rawDocPolicy = \_ -> newDC ("sweet" :: String) (<>)
                         , rawFieldPolicies = [
                         {-
                              ( "password", \doc -> let n' = at "name" doc :: String
                                                    in newDC n' n')
                              -}
                            ]
                         }
                       }

        x = [ "name" =: n
            , "password" =: p --(pu p :: PolicyLabeled DCLabel String)
            ] :: Document DCLabel
    {-
    lx <- applyRawPolicyP noPrivs c x
    ioTCB $ print lx
    ioTCB $ print x
    -}
    let act = do -- insertP noPrivs c x
                 y <- liftLIO $ applyRawPolicyP noPrivs c x
                 insertP noPrivs c y
    {-
                 liftLIO $ do lv <- label (newDC ("why"::String)
                                        ("me"::String)) "woo"
                              unlabel lv
                              -}

    accessP noPrivs pipe M.master db act
  close pipe
  putStrLn $ show res ++ (prettyShow l)
-- ##################################################################



--
-- Collections
--

-- | Name of collection
type CollectionName = M.Collection

-- | A collection is a MongoDB collection associated with a
-- label, clearance and labeling policy. The label
-- specifies who can write to a collection (i.e., only priciples whos
-- current label flows to the label of the
-- collection). The clearance limits
-- the sensitivity of the data written to the collection (i.e.,
-- the labels of all data in the collection must flow to the clearance).
data Collection l = Collection { colLabel  :: l
                               -- ^ Collection label
                               , colClear  :: l
                               -- ^ Collection clearance
                               , colIntern :: CollectionName
                               -- ^ Actual MongoDB collection
                               , colPolicy :: RawPolicy l
                               -- ^ Collection labeling policy
                               }
instance Label l => Show (Collection l) where
  show c = show "Collection "
              ++ show (colIntern c)
              ++ "\t" ++ show (colLabel c)
              ++ "\t" ++ show (colClear c)
           

-- | Create a collection given a collection label, clearance, name,
-- and policy. Note that the collection label and clearance must be
-- above the current label and below the current clearance.
collection :: LabelState l p s
           => l               -- ^ Collection label
           -> l               -- ^ Collection clearance
           -> CollectionName  -- ^ Collection name
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collection l c n pol = collectionP noPrivs l c n pol

-- | Same as 'collection', but uses privileges when comparing the
-- collection label and clearance with the current label and clearance.
collectionP :: LabelState l p s
           => p               -- ^ Privileges
           -> l               -- ^ Collection label
           -> l               -- ^ Collection clearance
           -> CollectionName  -- ^ Collection name
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collectionP p' l c n pol = withCombinedPrivs p' $ \p -> do
  aguardP p l
  aguardP p c
  return $ Collection { colLabel  = l
                      , colClear  = c
                      , colIntern = n
                      , colPolicy = pol
                      }

--
-- Databases
--


-- | Name of database
type DatabaseName = M.Database

-- | A database has a label, which is used to enforce who can write to
-- the database, and an internal identifier corresponding to the underlying
-- MongoDB database.
data Database l = Database { dbLabel  :: l      -- ^ Label of database
                           , dbIntern :: DatabaseName -- ^ Actual MongoDB 
                           } deriving (Eq, Show)

--
-- Policies 
--


-- | A @RawPolicy@ encodes a document policy, and all
-- field policies. It is required that all fields of type
-- 'PolicyLabled' have a field/column policy -- if using only this
-- low-level interface a runtime-error will occur if this is not
-- satisfied.
data RawPolicy l = RawPolicy {
      rawDocPolicy     :: Document l -> l
    -- ^ A row (document) policy is a function from a 'Document' to a 'Label'.
    , rawFieldPolicies :: [(Key, Document l -> l)]
    -- ^ A column (field) policy is a function from a 'Document' to a
    -- 'Label', for each field of type 'PolicyLabeled'.
  }


-- | Apply a raw field/column policy to the field corresponding to the
-- key. If the policy has not been specified for this key, the function
-- throws an exception. Similarly, if the policy has already been
-- applied for this key and the label existing label does not match the
-- newly policy-generated label, an exception is thrown.
-- It is required that the label of any 'Labeled' and 'PolicyLabeled'
-- values be below the clearnce of the collection (this is enforced in
-- 'applyRawPolicyP').
applyRawFieldPolicyP :: (LabelState l p s)
                     => p 
                     -> Collection l
                     -> Document l
                     -> Key
                     -> LIO l p s (Field l)
applyRawFieldPolicyP p col doc k = do
  let policies = rawFieldPolicies . colPolicy $ col
  -- Get the 'PolicyLabeled' value corresponding to k:
  plv <- getPolicyLabeledVal
  -- Find policy corresponding to key k:
  f <- maybe (throwIO NoFieldPolicy) return $ List.lookup k policies 
  -- Apply policy, or check matching labels:
  lv <- case plv of
         (PU v)  -> labelP p (f doc) v
         (PL lv) -> do unless (labelOf lv == f doc) $ throwIO PolicyViolation
                       return lv
  -- Return new field, with policy applied value
  return (k := (PolicyLabeledVal . PL $ lv))
    where getPolicyLabeledVal = case look k doc of
            (Just (PolicyLabeledVal x)) -> return  x
            _                           -> throwIO InvalidPolicy

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled'.
applyRawFieldPoliciesP :: (LabelState l p s)
                       => p 
                       -> Collection l
                       -> Document l
                       -> LIO l p s (Document l)
applyRawFieldPoliciesP p col doc = forM doc $ \field@(k := v) ->
  case v of
    (PolicyLabeledVal _) -> applyRawFieldPolicyP p col doc k
    _                    -> return field

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled', and then apply the raw document/row policy. It
-- must be that every labeled value in the document (including the
-- document itself) have a label that is below the clearance of
-- the collection. However, this is not checked by @applyRawPolicyP@.
-- Instead 'insert' (and similar operators) performs this check.
applyRawPolicyP :: (LabelState l p s)
                => p 
                -> Collection l
                -> Document l
                -> LIO l p s (LabeledDocument l)
applyRawPolicyP p' col doc = withCombinedPrivs p' $ \p -> do
  let docP = rawDocPolicy . colPolicy $ col
  -- Apply field/column policies:
  doc' <- applyRawFieldPoliciesP p col doc
  -- Apply document/row policy:
  labelP p (docP doc') doc'

-- | Same as 'applyRawPolicy', but ignores the current label and
-- clearance when applying policies.
applyRawPolicyTCB :: (LabelState l p s)
                  => Collection l
                  -> Document l
                  -> LIO l p s (LabeledDocument l)
applyRawPolicyTCB col doc = do
  -- Save current state:
  s0 <- getTCB
  -- Set state to most permissive label & clearance:
  setLabelTCB lbot
  lowerClrTCB ltop
  -- Apply policy to document:
  ldoc <- applyRawPolicyP noPrivs col doc
  -- Restore state:
  putTCB s0
  return ldoc

--
-- Exceptions
--

-- | Field/column policies are required for every 'PolicyLabled' value
-- in a document.
data PolicyError = NoFieldPolicy   -- ^ Policy for field no specified
                 | InvalidPolicy   -- ^ Policy application invalid
                 | PolicyViolation -- ^ Policy has been violated
  deriving (Typeable)

instance Show PolicyError where
  show NoFieldPolicy   = "NoFieldPolicy: Field policy not found"
  show InvalidPolicy   = "InvalidPolicy: Invalid policy application"
  show PolicyViolation = "PolicyViolation: Policy has been violated"

instance E.Exception PolicyError


--
-- Monad
--

-- | Since it would be a security violation to make 'LIO' an instance
-- of @MonadIO@, we create a Mongo-specific, non-exported,  wrapper for
-- 'LIO' that is instance of @MonadIO@.
--
-- NOTE: IT IS IMPORTANT THAT @UnsafeLIO@ REMAINS HIDDEN AND NO
-- EXPORTED WRAPPER BE MADE AN INSTATNCE OF @MonadLIO@.
newtype UnsafeLIO l p s a = UnsafeLIO { unUnsafeLIO :: LIO l p s a }
  deriving (Functor, Applicative, Monad)

-- | UNSAFE: Instance of @MonadIO@.
instance LabelState l p s => MonadIO (UnsafeLIO l p s) where
  liftIO = UnsafeLIO . rtioTCB

-- | UNSAFE: Instance of @MonadBase IO@.
instance LabelState l p s => MonadBase IO (UnsafeLIO l p s) where
  liftBase = UnsafeLIO . rtioTCB

-- | UNSAFE: Instance of @MonadBaseControl IO@.
-- NOTE: This instance is a hack. I got this to work by tweaking Bas'
-- Annex example, but should spend time actually understanding the
-- details.
instance LabelState l p s => MonadBaseControl IO (UnsafeLIO l p s) where
  newtype StM (UnsafeLIO l p s) a = StUnsafeLIO {
     unStUnsafeLIO :: (StM (StateT (LIOstate l p s) IO) a) }
  liftBaseWith f = UnsafeLIO . LIO $ liftBaseWith $ \runInIO ->
                     f $ liftM StUnsafeLIO . runInIO
                             . (\(LIO x) -> x) .  unUnsafeLIO
  restoreM = UnsafeLIO . LIO . restoreM . unStUnsafeLIO

-- | Instance of @MonadIO@.
instance LabelState l p s => MonadLIO (UnsafeLIO l p s) l p s where
  liftLIO = UnsafeLIO

-- | An LIO action with MongoDB access.
newtype LIOAction l p s a =
    LIOAction { unLIOAction :: M.Action (UnsafeLIO l p s) a }
  deriving (Functor, Applicative, Monad)

newtype Action l p s a = Action (ReaderT (Database l) (LIOAction l p s) a)
  deriving (Functor, Applicative, Monad)

instance LabelState l p s => MonadLIO (LIOAction l p s) l p s where
  liftLIO = LIOAction . liftLIO

instance LabelState l p s => MonadLIO (Action l p s) l p s where
  liftLIO = Action . liftLIO

-- | Lift a MongoDB action into 'Action' monad.
liftAction :: LabelState l p s => M.Action (UnsafeLIO l p s) a -> Action l p s a
liftAction = Action . lift . LIOAction 

-- | Run action against database on server at other end of pipe. Use
-- access mode for any reads and writes. Return 'Left' on connection
-- failure or read/write failure.
-- The current label is raised to the the join of the database label
-- and current label.
--
-- TODO: make sure that Failure does not leak any information.
access :: LabelState l p s
       => Pipe
       -> AccessMode
       -> Database l
       -> Action l p s a
       -> LIO l p s (Either Failure a)
access = accessP noPrivs

-- | Same as 'access', but uses privileges when raising the current
-- label.
accessP :: LabelState l p s
        => p 
        -> Pipe
        -> AccessMode
        -> Database l
        -> Action l p s a
        -> LIO l p s (Either Failure a)
accessP p' pipe mode db (Action act) = withCombinedPrivs p' $ \p -> do 
  taintP p (dbLabel db)
  let lioAct = runReaderT act db
  unUnsafeLIO $ M.access pipe mode (dbIntern db) (unLIOAction lioAct)


--
-- Write 
--

-- | Class used to overload inserting labeled and unlabeled documents
-- into a collection. Only the definition for @inserP@ is needed.
class Label l => Insert l doc where
  -- | Insert document into collection and return its @_id@ value,
  -- which is created automatically if not supplied.
  insert :: (LabelState l p s, Serialize l)
         => Collection l
         -> doc
         -> Action l p s M.Value
  insert = insertP noPrivs
  -- | Same as 'insert' except it does not return @_id@
  insert_ :: (LabelState l p s, Serialize l)
          => Collection l
          -> doc
          -> Action l p s ()
  insert_ c d = insert c d >> return ()
  -- | Same as 'insert', but uses privileges when applying the
  -- collection policies, and doing label comparisons.
  insertP :: (LabelState l p s, Serialize l)
          => p 
          -> Collection l
          -> doc
          -> Action l p s M.Value
  -- | Same as 'insertP' except it does not return @_id@
  insertP_ :: (LabelState l p s, Serialize l)
           => p 
           -> Collection l
           -> doc
           -> Action l p s ()
  insertP_ p c d = insertP p c d >> return ()


instance Label l => Insert l (Document l) where
  insertP p' col doc = do
    let colC = colClear col
    db <- Action $ ask
    ldoc <- liftLIO $ withCombinedPrivs p' $ \p -> do
              -- Check that we can write to database:
              wguardP p (dbLabel db)
              -- Check that we can write to collection:
              wguardP p (colLabel col)
              -- Apply policies (data should not be labeled with a label
              -- that is above the collection clearance):
              ldoc <- withClearance colC $ applyRawPolicyP p col doc
              -- Check that 'Labeled' values have labels below clearnce:
              guardLabeledVals (unlabelTCB ldoc) colC
              -- Policies applied & labels are below clearance:
              return ldoc
    let bsonDoc = toBsonDoc . unlabelTCB $ ldoc
    liftAction $ M.useDb (dbIntern db) $ M.insert (colIntern col) bsonDoc
      where guardLabeledVals []            _ = return ()
            guardLabeledVals ((_ := v):ds) c = do
              case v of
                (LabeledVal lv) -> unless (labelOf lv `leq` c) $
                                     throwIO LerrClearance
                _               -> return ()

instance Label l => Insert l (Labeled l (Document l)) where
  insertP p' col ldoc = do
    db <- Action $ ask
    liftLIO $ withCombinedPrivs p' $ \p -> do
      -- Check that we can write to database:
      wguardP p (dbLabel db)
      -- Check that we can write to collection:
      wguardP p (colLabel col)
      -- Check that the labels of all labeled values are below
      -- clearance, check that 'PolicyLabeled' values match, and check that
      -- the label of the document is policy-generated and below clearance:
      guardAll
    let bsonDoc = toBsonDoc . unlabelTCB $ ldoc
    liftAction $ M.useDb (dbIntern db) $ M.insert (colIntern col) bsonDoc
      where colC = colClear col
            --
            doc = unlabelTCB ldoc
            --
            guardAll = do
              -- Apply policy to document:
              ldoc' <- applyRawPolicyTCB col doc
              -- Check that document labels match:
              unless (labelOf ldoc' == labelOf ldoc) $ throwIO PolicyViolation
              -- Check that the document label is below collection clerance:
              unless (labelOf ldoc `leq` colC) $ throwIO LerrClearance
              -- Check that fields match and are below collection clearance.
              -- Fields are protected by document label, so if an
              -- exception is thrown it should have this label.
              guardFields (unlabelTCB ldoc') doc
            --
            guardFields []               []               = return ()
            guardFields ((k0 := v0):ds0) ((k1 := v1):ds1) = 
              unless (k0 == k1 && v0 `eq` v1) $ throwViolation
            guardFields _                _                = throwViolation
            --
            eq (BsonVal v1)           (BsonVal v2)           = v1 == v2
            eq (LabeledVal lv1)       (LabeledVal lv2)       = lv1 `eqL` lv2
            eq (PolicyLabeledVal lv1) (PolicyLabeledVal lv2) = lv1 `eqPL` lv2
            eq _                      _                      = False
            --
            eqL lv1 lv2 = (labelOf lv1 == labelOf lv2) &&
                          (unlabelTCB lv1 == unlabelTCB lv2)
            --
            eqPL (PL lv1) (PL lv2) = lv1 `eqL` lv2
            eqPL _        _        = False
            --
            throwViolation = ioTCB $ E.throwIO $ LabeledExceptionTCB
                                (labelOf ldoc) (E.toException PolicyViolation)

--
-- Read
--

-- | Use select to create a basic query with defaults, then modify if
-- desired. Example: @(select sel col) {limit = 10}@. Note that unlike
-- MongoDB's query functionality, our queries do not allow for
-- projections (since policies may need a field that is not projects).
--
-- TODO: add snapshot, hints, sorts (with correct tainting), etc.
data Query l = Query { options :: [QueryOption]
                     , selection :: Selection l
                     , skip  :: Word32
                     -- ^ Number of documents to skip, default 0.
                     , limit :: Limit
                     -- ^ Max number of documents to return. Default, 0,
                     -- means no limit.
                     , batchSize :: BatchSize
                     -- ^ The number of document to return in each
                     -- batch response from the server. 0 means
                     -- Mongo default.
                     } deriving (Show)

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
  deriving (Show)

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
                             , coll :: Collection l -- ^ Collection operaing on
                             } deriving (Show)

-- | Convert a 'Selection' to the mongoDB equivalent.
selectionToMSelection :: (Serialize l, Label l) => Selection l -> M.Selection
selectionToMSelection s = M.Select { M.selector = toBsonDoc $ selector s
                                   , M.coll = colIntern (coll s) }

-- | Convert a 'Selector' to a 'Selection' or 'Query'
class Select selectionOrQuery where
  select :: Label l => Selector l -> Collection l -> selectionOrQuery l
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

-- | Fetch documents satisfying query. A labeled 'Cursor' is returned,
-- which can be used to retrieve the actual 'Document's.
simpleFindP :: (Serialize l, LabelState l p s)
            => p -> SimpleQuery l -> Action l p s (SimpleCursor l)
simpleFindP p' (SimpleQuery q) = do
  db <- Action $ ask
  let col = coll . selection $ q
  liftLIO $ withCombinedPrivs p' $ \p -> do
     -- Check that we can read from database:
     taintP p (dbLabel db)
     -- Check that we can read from collection:
     taintP p (colLabel col)
  cur <- liftAction $ M.useDb (dbIntern db) $ M.find (queryToMQuery q)
  return . SimpleCursor $ Cursor { curLabel  = (colLabel col) `lub` (dbLabel db)
                                 , curIntern = cur
                                 , curCol    = col
                                 }

--
-- Cursor
--


-- | A labeled cursor. The cursor is labeled with the join of the
-- database and collection it reads from.
data Cursor l = Cursor { curLabel :: l -- ^ Cursorlabel
                       , curIntern :: M.Cursor  -- ^ Actual cursor
                       , curCol :: Collection l -- ^ Corresponding collection
                       } 

-- | A simple cursor corresponds to a 'SimpleQuery'.
newtype SimpleCursor l = SimpleCursor (Cursor l)

-- | Return next document in query result, or @Nothing@ if finished.
-- The current label is raised to join of the current label and
-- 'Cursor' label. The document is labeled according to the
-- underlying 'Collection'\'s policies.
simpleNext :: (LabelState l p s, Serialize l)
           => SimpleCursor l
           -> Action l p s (Maybe (LabeledDocument l))
simpleNext = simpleNextP noPrivs

-- | Same as 'simpleNext', but usess privileges raising the current label.
simpleNextP :: (LabelState l p s, Serialize l)
            => p
            -> SimpleCursor l
            -> Action l p s (Maybe (LabeledDocument l))
simpleNextP p' (SimpleCursor cur) = do
  liftLIO $ withCombinedPrivs p' $ \p -> taintP p (curLabel cur)
  md <- fromBsonDoc' <$> (liftAction $ M.next (curIntern cur))
  case md of
    Nothing -> return Nothing
    Just d -> liftLIO $ withCombinedPrivs p' $ \p -> do
      Just <$> applyRawPolicyTCB (curCol cur) d
    where fromBsonDoc' = maybe Nothing fromBsonDocStrict


--
-- Serializing 'Value's
--

-- | Convert a 'Document' to a Bson @Document@. It is an error to call
-- this function with malformed 'Document's (i.e., those for which
-- a policy has not been applied.
toBsonDoc :: (Serialize l, Label l) => Document l -> M.Document
toBsonDoc = map (\(k := v) -> (k M.:= toBsonValue v)) . exceptInternal

-- | Convert a Bson @Document@ to a 'Document'. This implementation is
-- relaxed and omits any fields that were not converted. Use the
-- 'fromBsonDocStrict' for a strict conversion. 
fromBsonDoc :: (Serialize l, Label l) => M.Document -> Document l
fromBsonDoc d = 
  let cs' = map (\(k M.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
  in exceptInternal $ cs

-- | Same as 'fromBsonDoc', but fails (returns @Nothing@) if any of
-- the field  values failed to be serialized.
fromBsonDocStrict :: (Serialize l, Label l) => M.Document -> Maybe (Document l)
fromBsonDocStrict d = 
  let cs' = map (\(k M.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
      ok  = all (isJust .snd) cs'
  in if ok then Just . exceptInternal $ cs else Nothing


-- | Remove any fields from the document that have
-- 'hailsInternalKeyPrefix' as a prefix
exceptInternal :: Label l => Document l -> Document l
exceptInternal [] = []
exceptInternal (f@(k := _):fs) =
  let rest = exceptInternal fs
  in if hailsInternalKeyPrefix `isPrefixOf` k
       then rest
       else f:rest
                                


-- | This prefix is reserved for HAILS keys. It should not be used by
-- arbitrary code.
hailsInternalKeyPrefix :: M.Label
hailsInternalKeyPrefix = u "__hails_internal_"

-- | Serializing a 'Labeled' to a BSON @Document@ with key 
-- @lBsonLabeledValKey@.
lBsonLabeledValKey :: M.Label
lBsonLabeledValKey = hailsInternalKeyPrefix `append` u "Labeled"

-- | Serializing a 'PolicyLabeled' to a BSON @Document@ with key 
-- @lBsonPolicyLabeledValKey@.
lBsonPolicyLabeledValKey :: M.Label
lBsonPolicyLabeledValKey = hailsInternalKeyPrefix `append` u "PolicyLabeled"

-- | When serializing a 'Labeled' we serialize it to a document
-- containing the label and value, the key for the label is
-- @lBsonLabelKey@.
lBsonLabelKey :: M.Label
lBsonLabelKey = u "label"

-- | When serializing a 'Labeled' (or 'PolicyLabeled') we serialize
-- it to a document containing the value, the key for the value
-- is @lBsonValueKey@.
lBsonValueKey :: M.Label
lBsonValueKey = u "value"

-- | Convert 'Value' to Bson @Value@
toBsonValue :: (Serialize l, Label l) => Value l -> M.Value
toBsonValue mV = 
  case mV of 
    (BsonVal v)            -> v
    (LabeledVal lv) -> M.val [ lBsonLabeledValKey M.=:
              [ lBsonLabelKey M.=: Binary (encode (labelOf lv))
              , lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PL lv)) -> M.val [ lBsonPolicyLabeledValKey M.=:
              [ lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PU _)) -> error "bsonValue2lbsonValue: Invalid use."

-- | Convert Bson @Value@ to 'Value'
fromBsonValue :: (Serialize l, Label l) => M.Value -> Maybe (Value l)
fromBsonValue mV = do
  case mV of
    x@(M.Doc d) ->
      let haveL = isJust $ M.look lBsonLabeledValKey d
          havePL = isJust $ M.look lBsonPolicyLabeledValKey d
      in if haveL || havePL
           then getLabeled d `orMaybe` getPolicyLabeled d
           else Just (BsonVal x)
    x         -> Just (BsonVal x)
  where getLabeled :: (Serialize l, Label l) => M.Document -> Maybe (Value l)
        getLabeled d = do
          (M.Doc lv) <- M.look lBsonLabeledValKey d
          (Binary b) <- M.lookup lBsonLabelKey lv
          l <- either (const Nothing) return (decode b)
          v <- M.look lBsonValueKey lv
          return . LabeledVal $ labelTCB l v
        --
        getPolicyLabeled :: (Serialize l, Label l) => M.Document -> Maybe (Value l)
        getPolicyLabeled d = do
          (M.Doc lv) <- M.look lBsonPolicyLabeledValKey d
          v <- M.look lBsonValueKey lv
          return . PolicyLabeledVal . PU $ v
        --
        orMaybe :: Maybe a -> Maybe a -> Maybe a
        orMaybe x y = if isJust x then x else y
