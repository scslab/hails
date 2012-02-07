{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Hails.Database.MongoDB.TCB.Types where

import LIO
import LIO.TCB ( unlabelTCB
							 , labelTCB
							 , rtioTCB)
import qualified Database.MongoDB as M
import Hails.Data.LBson.TCB
import qualified Control.Exception as E
import Data.Maybe
import Data.Typeable
import Data.CompactString.UTF8 (append, isPrefixOf)
import Data.Serialize (Serialize, encode, decode)

import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.IO.Class as IO

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
                               , colPolicy :: RawPolicy l
                               -- ^ Collection labeling policy
                               }

--
-- Databases
--


-- | Name of database
type DatabaseName = M.Database

-- | A database has a label, which is used to enforce who can write to
-- the database, and an internal identifier corresponding to the underlying
-- MongoDB database.
data Database l = Database { dbLabel  :: l
                           -- ^ Label of database
                           , dbIntern :: DatabaseName
                           -- ^ Actual MongoDB
                           , dbColPolicies :: [(CollectionName, Collection l)]
                           }

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

--
-- Exceptions
--

-- | Field/column policies are required for every 'PolicyLabled' value
-- in a document.
data PolicyError = NoFieldPolicy   -- ^ Policy for field not specified
                 | InvalidPolicy   -- ^ Policy application invalid
                 | NoColPolicy     -- ^ Policy for Collection not specified
                 | PolicyViolation -- ^ Policy has been violated
  deriving (Typeable)

instance Show PolicyError where
  show NoFieldPolicy    = "NoFieldPolicy: Field policy not found"
  show NoColPolicy      = "NoColPolicy: Collection policy not found"
  show InvalidPolicy    = "InvalidPolicy: Invalid policy application"
  show PolicyViolation  = "PolicyViolation: Policy has been violated"

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

-- | An LIO action with MongoDB access.
newtype LIOAction l p s a =
    LIOAction { unLIOAction :: M.Action (UnsafeLIO l p s) a }
  deriving (Functor, Applicative, Monad)

newtype Action l p s a = Action (ReaderT (Database l) (LIOAction l p s) a)
  deriving (Functor, Applicative, Monad)

instance LabelState l p s => MonadLIO (UnsafeLIO l p s) l p s where
  liftLIO = UnsafeLIO

instance LabelState l p s => MonadLIO (LIOAction l p s) l p s where
  liftLIO = LIOAction . liftLIO

instance LabelState l p s => MonadLIO (Action l p s) l p s where
  liftLIO = Action . liftLIO

-- | Lift a MongoDB action into 'Action' monad.
liftAction :: LabelState l p s => M.Action (UnsafeLIO l p s) a -> Action l p s a
liftAction = Action . lift . LIOAction

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
