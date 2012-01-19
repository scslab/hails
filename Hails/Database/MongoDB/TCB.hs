{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- TODO: remove:
{-# LANGUAGE OverloadedStrings #-}
--
module Hails.Database.MongoDB.TCB where

import Prelude hiding (lookup)
import LIO
import LIO.TCB (unlabelTCB, labelTCB)
import LIO.MonadCatch
import Hails.Data.LBson.TCB

import Data.Typeable
import qualified Data.List as List
import Data.Functor (Functor)

import Data.Maybe
import Data.Serialize
import Data.CompactString.UTF8 (append, isPrefixOf)

import Database.MongoDB.Connection
import Database.MongoDB ( Failure(..)
                        , AccessMode(..)
                        , MonadDB(..)
                        )
import qualified Database.MongoDB as M

import qualified Control.Exception as E
import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad (unless, forM, liftM)
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.IO.Class as IO


-- TODO: remove
import LIO.DCLabel
import DCLabel.PrettyShow
import LIO.TCB (ioTCB)

db :: Database DCLabel
db = Database lbot "baseball"

main :: IO ()
main =  do
  pipe <- runIOE $ connect(host "127.0.0.1")
  (res,l) <- evalDC $ do
    let n = "w00t" :: String
        p = "p455w0rd" :: String
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
    let act = do i <- insertP noPrivs c x
    {-
                 liftLIO $ do lv <- label (newDC ("why"::String)
                                        ("me"::String)) "woo"
                              unlabel lv
                              -}
                 return i

    accessP noPrivs pipe M.master db act
  close pipe
  putStrLn $ show res ++ (prettyShow l)
--



--
-- Collections
--

-- | Name of collection
type CollectionName = M.Collection

-- | A collection is a MongoDB collection associated with
-- label and clearance and labeling policy for collection documents.
-- The label of the collection is used to enforce who can write
-- to it (i.e., current label must flow to the label of the
-- collection). The clearance of the collection is used to limit
-- the sensitivity of the data written to the collection (i.e.,
-- label of data must flow to the clearance of the collection). The
-- label on the collection label and clearance is the current label,
-- i.e., they are public.
data Collection l = Collection { colLabel  :: l
                               -- ^ Collection label
                               , colClear  :: l
                               -- ^ Collection clearance
                               , colIntern :: CollectionName
                               -- ^ Actual MongoDB collection
                               , colPolicy :: RawPolicy l
                               -- ^ Collection labeling policy
                               }

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
                           }

--
-- Policies 
--


-- | A @RawPolicy@ encodes a document/row policy, and all
-- field/column policies. It is required that all fields of type
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
-- It is required that the label of any 'PolicyLabeled' value be below
-- the clearnce of the collection (this is enforce in 'applyRawPolicyP').
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
-- 'PolicyLabeled', and then apply the raw document/row policy It
-- must be that every labeled value in the document (including the
-- document itself) have a label that is below the clearance of
-- the collection.
applyRawPolicyP :: (LabelState l p s)
                => p 
                -> Collection l
                -> Document l
                -> LIO l p s (Labeled l (Document l))
applyRawPolicyP p' col doc = withCombinedPrivs p' $ \p -> do
  let colC = colClear col
      docP = rawDocPolicy . colPolicy $ col
  -- Apply policies
  withClearance colC $ do
    -- Apply field/column policies:
    doc' <- applyRawFieldPoliciesP p col doc
    -- Check that 'Labeled' values have labels below clearnce:
    guardLabeledVals doc' colC
    -- Apply document/row policy:
    labelP p (docP doc') doc'
    where guardLabeledVals []            _ = return ()
          guardLabeledVals ((_ := v):ds) c = do
            case v of
              (LabeledVal lv) -> unless (labelOf lv `leq` c) $
                                   throwIO LerrClearance
              _               -> return ()
            guardLabeledVals ds c

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

-- | Instance of @MonadIO@.
instance LabelState l p s => MonadIO (UnsafeLIO l p s) where
  liftIO = UnsafeLIO . ioTCB

-- | Instance of @MonadIO@.
instance LabelState l p s => MonadLIO (UnsafeLIO l p s) l p s where
  liftLIO = UnsafeLIO

-- | An LIO action with MongoDB access.
newtype LIOAction l p s a = LIOAction { unLIOAction :: M.Action (UnsafeLIO l p s) a }
  deriving (Functor, Applicative, Monad)

newtype Action l p s a = Action (ReaderT (Database l) (LIOAction l p s) a)
  deriving (Functor, Applicative, Monad)

instance LabelState l p s => MonadLIO (LIOAction l p s) l p s where
  liftLIO = LIOAction . liftLIO

instance LabelState l p s => MonadLIO (Action l p s) l p s where
  liftLIO = Action . liftLIO

-- TODO: make sure that exceptions are propagated properly and Failure
-- does not leak any information

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

-- | Lift a MongoDB action into 'Action' monad.
liftAction :: LabelState l p s => M.Action (UnsafeLIO l p s) a -> Action l p s a
liftAction = Action . lift . LIOAction 

-- | Insert document into collection and return its "_id" value,
-- which is created automatically if not supplied.
insertP :: (LabelState l p s, Serialize l)
        => p 
        -> Collection l
        -> Document l
        -> Action l p s M.Value
insertP p' col doc = do
  db <- Action $ ask
  ldoc <- liftLIO $ withCombinedPrivs p' $ \p -> do
            -- Check that we can write to database:
            wguardP p (dbLabel db)
            -- Check that we can write to collection:
            wguardP p (colLabel col)
            -- Apply policies:
            applyRawPolicyP p col doc
  let bsonDoc = toBsonDoc . unlabelTCB $ ldoc
  liftAction $ M.useDb (dbIntern db) $ M.insert (colIntern col) bsonDoc


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
exceptInternal (f@(k := v):fs) =
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
toBsonValue v = 
  case v of 
    (BsonVal v)            -> v
    (LabeledVal lv) -> M.val [ lBsonLabeledValKey M.=:
              [ lBsonLabelKey M.=: Binary (encode (labelOf lv))
              , lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PL lv)) -> M.val [ lBsonPolicyLabeledValKey M.=:
              [ lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PU _)) -> error "bsonValue2lbsonValue: Invalid use."

-- | Convert Bson @Value@ to 'Value'
fromBsonValue :: (Serialize l, Label l) => M.Value -> Maybe (Value l)
fromBsonValue v = do
  case v of
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
