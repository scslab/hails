{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE DeriveDataTypeable,
             GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module Hails.Database.MongoDB.TCB.Types ( -- * Collection
                                          CollectionName
                                        , CollectionMap
                                        , CollectionPolicy(..)
                                        , Collection(..)
                                        , collection, collectionP, collectionTCB
                                          -- * Database
                                        , DatabaseName
                                        , Database(..)
                                        , database, databaseP, databaseTCB
                                        , assocCollection, assocCollectionP
                                        , assocCollectionTCB 
                                          -- * Policies
                                        , RawPolicy(..)
                                        , PolicyError(..)
                                          -- * Monad
                                        , UnsafeLIO(..)
                                        , LIOAction(..)
                                        , Action(..)
                                        , liftAction
                                        , getDatabase, putDatabase
                                        -- * Serializing Value
                                        , toBsonDoc
                                        , fromBsonDoc, fromBsonDocStrict
                                        ) where

import LIO
import LIO.TCB ( unlabelTCB
               , labelTCB
               , rtioTCB )
import qualified Database.MongoDB as M
import Hails.Data.LBson.TCB
import Data.Maybe
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.CompactString.UTF8 (append, isPrefixOf)
import Data.Serialize (Serialize, encode, decode)

import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad.State.Strict hiding (liftIO)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Exception as E

--
-- Collections
--

-- | A collection policy is is a label,
-- clearance and labeling policy. The label specifies who can write to a
-- collection (i.e., only computatoin whose current label flows to the
-- label of the collection). The clearance limits the sensitivity of the
-- data written to the collection (i.e., the labels of all data in the
-- collection must flow to the clearance). Note that the collection label
-- does /not/ impose a restriction on the data (i.e., data can have
-- high integrity). The collection policy specifies the policies for
-- labeling documents and fields of documents.
data Collection l = Collection { colIntern :: CollectionName
                               -- ^ Collection name
                               , colSec    :: CollectionPolicy l
                               -- ^ Collection secutiry policies: access control
                               -- and labeling policies
                               }

-- | Labels and policies associated with a collection. See 'Collection'.
data CollectionPolicy l = CollectionPolicy { colLabel   :: l
                                           -- ^ Collection label
                                           , colClear  :: l
                                           -- ^ Collection clearance
                                           , colPolicy :: RawPolicy l
                                           -- ^ Collection labeling policy
                                           }

-- | Name of collection
type CollectionName = M.Collection

-- | Create a collection given a collection name, label, clearance, 
-- and policy. Note that the collection label and clearance must be
-- above the current label and below the current clearance.
collection :: LabelState l p s
           => CollectionName  -- ^ Collection name
           -> l               -- ^ Collection label
           -> l               -- ^ Collection clearance
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collection = collectionP noPrivs

-- | Same as 'collection', but uses privileges when comparing the
-- collection label and clearance with the current label and clearance.
collectionP :: LabelState l p s
           => p               -- ^ Privileges
           -> CollectionName  -- ^ Collection name
           -> l               -- ^ Collection label
           -> l               -- ^ Collection clearance
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collectionP p' n l c pol = withCombinedPrivs p' $ \p -> do
  aguardP p l
  aguardP p c
  collectionTCB n l c pol

-- | Same as 'collection', but ignores IFC.
collectionTCB :: LabelState l p s
              => CollectionName  -- ^ Collection name
              -> l               -- ^ Collection label
              -> l               -- ^ Collection clearance
              -> RawPolicy l     -- ^ Collection policy
              -> LIO l p s (Collection l)
collectionTCB n l c pol = 
  return $ Collection { colIntern = n
                      , colSec    = CollectionPolicy { colLabel = l
                                                     , colClear = c
                                                     , colPolicy = pol }
                      }

  

--
-- Databases
--


-- | Name of database
type DatabaseName = M.Database

-- | A labeled 'Collection' map.
type CollectionMap l = Labeled l (Map CollectionName (CollectionPolicy l))

-- | A database has a label, which is used for controlling access to
-- the database, an internal identifier corresponding to the underlying
-- MongoDB database, and a set of 'Collection's protected by a label.
data Database l = Database
  { dbIntern :: DatabaseName
    -- ^ Actual MongoDB
  , dbLabel  :: l
    -- ^ Label of database
  , dbColPolicies :: CollectionMap l
    -- ^ Collections associated with databsae
  }


-- | Create a 'Database'. Given a set of privileges, the name of the
-- database, the database label, and set of collections, create a
-- database. Note that this does not restrict an application from
-- creating arbitrary databases and collections---this should be
-- handled by a shim layer.
databaseP :: LabelState l p s
          => p                    -- ^ Privileges
          -> DatabaseName         -- ^ Name of database
          -> l                    -- ^ Label of database
          -> CollectionMap l      -- ^ Labeled colleciton map
          -> LIO l p s (Database l)
databaseP p' n l cs = withCombinedPrivs p' $ \p -> do
  aguardP p l
  databaseTCB n l cs

-- | Sameas 'databaseP', but ignores IFC checks.
databaseTCB :: LabelState l p s
            => DatabaseName
            -> l
            -> CollectionMap l
            -> LIO l p s (Database l)
databaseTCB n l cs = return $ Database { dbIntern      = n
                                       , dbLabel       = l
                                       , dbColPolicies = cs
                                       }

-- | Same as 'databaseP', but does not use privileges when comparing
-- the current label (and clearance) with the supplied database label.
database :: LabelState l p s
         => DatabaseName
         -> l
         -> CollectionMap l
         -> LIO l p s (Database l)
database = databaseP noPrivs


-- | Associate a collection with the underlying database.
assocCollectionP :: LabelState l p s
                 => p
                 -> Collection l
                 -> Action l p s ()
assocCollectionP p' (Collection n cp) = do
  db <- getDatabase
  (l, colMap) <- liftLIO $ withCombinedPrivs p' $ \p -> do
    let colMap = unlabelTCB $ dbColPolicies db
        l = labelOf $ dbColPolicies db
    wguardP p l
    return (l, colMap)
  let dCPs = Map.insert n cp colMap
  putDatabase $ db { dbColPolicies = labelTCB l dCPs }

-- | Same as 'assocCollectionP', but does not use privileges when
-- writing to database collection map.
assocCollection :: LabelState l p s
                => Collection l
                -> Action l p s ()
assocCollection = assocCollectionP noPrivs

-- | Same as 'assocCollectionP', but ignores IFC.
assocCollectionTCB :: LabelState l p s
                   => Collection l
                   -> Action l p s ()
assocCollectionTCB (Collection n cp) = do
  db <- getDatabase
  let colMap = unlabelTCB $ dbColPolicies db
      l = labelOf $ dbColPolicies db
  let dCPs = Map.insert n cp colMap
  putDatabase $ db { dbColPolicies = labelTCB l dCPs }


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
-- of @MonadIO@, we create a Mongo-specific,  wrapper for
-- 'LIO' that is instance of @MonadIO@.
--
-- NOTE: IT IS IMPORTANT THAT @UnsafeLIO@ NEVER BE EXPOSED BY MODULES
-- THAT ARE NOT Unsafe.
newtype UnsafeLIO l p s a = UnsafeLIO { unUnsafeLIO :: LIO l p s a }
  deriving (Functor, Applicative, Monad)

-- | UNSAFE: Instance of @MonadIO@.
instance LabelState l p s => MonadIO (UnsafeLIO l p s) where
  liftIO = UnsafeLIO . rtioTCB

-- | An LIO action with MongoDB access.
newtype LIOAction l p s a =
    LIOAction { unLIOAction :: M.Action (UnsafeLIO l p s) a }
  deriving (Functor, Applicative, Monad)

newtype Action l p s a = Action (StateT (Database l) (LIOAction l p s) a)
  deriving (Functor, Applicative, Monad)

instance LabelState l p s => MonadLIO (UnsafeLIO l p s) l p s where
  liftLIO = UnsafeLIO

instance LabelState l p s => MonadLIO (LIOAction l p s) l p s where
  liftLIO = LIOAction . liftLIO

instance LabelState l p s => MonadLIO (Action l p s) l p s where
  liftLIO = Action . liftLIO

-- | Get underlying database.
getDatabase :: Action l p s (Database l)
getDatabase = Action $ get

-- | Replace underlying databse
putDatabase :: Database l -> Action l p s ()
putDatabase db = Action $ put db

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
