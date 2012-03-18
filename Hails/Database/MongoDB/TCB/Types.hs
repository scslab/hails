{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE DeriveDataTypeable,
             GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies #-}
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
                                        , FieldPolicy(..)
                                        , isSearchableField
                                        , searchableFields
                                        , PolicyError(..)
                                        , NoSuchDatabaseError(..)
                                          -- * Monad
                                        , UnsafeLIO(..)
                                        , LIOAction(..)
                                        , Action(..)
                                        , liftAction
                                        , getDatabase
                                        -- * Cursor
                                        , Cursor(..)
                                        -- * Misc
                                        , Failure
                                        ) where

import LIO
import LIO.TCB ( LIO(..)
               , LIOstate
               , unlabelTCB
               , labelTCB
               , rtioTCB )

import qualified Database.MongoDB as M
import Database.MongoDB (Failure)

import Hails.Data.LBson.TCB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad.Reader hiding (liftIO)
import Control.Monad.State hiding (liftIO)
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
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
                 -> Database l
                 -> LIO l p s (Database l)
assocCollectionP p' (Collection n cp) db = do
  (l, colMap) <- liftLIO $ withCombinedPrivs p' $ \p -> do
    let colMap = unlabelTCB $ dbColPolicies db
        l = labelOf $ dbColPolicies db
    wguardP p l
    return (l, colMap)
  let dCPs = Map.insert n cp colMap
  return $ db { dbColPolicies = labelTCB l dCPs }

-- | Same as 'assocCollectionP', but does not use privileges when
-- writing to database collection map.
assocCollection :: LabelState l p s
                => Collection l
                -> Database l
                -> LIO l p s (Database l)
assocCollection = assocCollectionP noPrivs

-- | Same as 'assocCollectionP', but ignores IFC.
assocCollectionTCB :: LabelState l p s
                   => Collection l
                   -> Database l
                   -> LIO l p s (Database l)
assocCollectionTCB (Collection n cp) db = do
  let colMap = unlabelTCB $ dbColPolicies db
      l = labelOf $ dbColPolicies db
  let dCPs = Map.insert n cp colMap
  return $ db { dbColPolicies = labelTCB l dCPs }


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
    , rawFieldPolicies :: [(Key, FieldPolicy l)]
    -- ^ A column (field) policy is a function from a 'Document' to a
    -- 'Label', for each field of type 'PolicyLabeled'.
  }

-- | A @FieldPolicy@ specifies the policy-generated label of
-- a field. @SearchabelField@ specifies that the field can be
-- referenced in the selection clause of a @Query@, and therefore
-- the document label does not apply to it.
data FieldPolicy l = SearchableField
                   | FieldPolicy (Document l -> l)

-- | Returns True if the policy is for a searchable field
isSearchableField :: FieldPolicy l -> Bool
isSearchableField SearchableField = True
isSearchableField _ = False

-- | Returns a list of the @SearchableField@s speicified in a
-- @RawPolicy@
searchableFields :: RawPolicy l -> [Key]
searchableFields policy = map fst $
  filter (\(_, f) -> isSearchableField f) fields
  where fields = rawFieldPolicies policy

--
-- Exceptions
--

-- | Field/column policies are required for every 'PolicyLabled' value
-- in a document.
data PolicyError = NoFieldPolicy   -- ^ Policy for field not specified
                 | InvalidPolicy   -- ^ Policy application invalid
                 | NoColPolicy     -- ^ Policy for Collection not specified
                 | InvalidFieldPolicyType
                 -- ^ Field with associated policy is not of PolicyLabeled type
                 | InvalidSearchableType
                 -- ^ Searchable fields cannot contain labeled values
                 | PolicyViolation -- ^ Policy has been violated
  deriving (Typeable)

instance Show PolicyError where
  show NoFieldPolicy          = "NoFieldPolicy: Field policy not found"
  show NoColPolicy            = "NoColPolicy: Collection policy not found"
  show InvalidPolicy          = "InvalidPolicy: Invalid policy application"
  show PolicyViolation        = "PolicyViolation: Policy has been violated"
  show InvalidFieldPolicyType = "InvalidFieldPolicyType: " ++
                                "Expected \'PolicyLabeled\' type"
  show InvalidSearchableType  = "InvalidSearchableType: Searchable" ++
                                "fields cannot contain labeled values"

instance E.Exception PolicyError

data NoSuchDatabaseError = NoSuchDatabase
  deriving (Typeable)

instance Show NoSuchDatabaseError where
  show NoSuchDatabase = "NoSuchDatabase: No such database exists"

instance E.Exception (NoSuchDatabaseError)

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

-- | Get underlying database.
getDatabase :: Action l p s (Database l)
getDatabase = Action $ ask


-- | Lift a MongoDB action into 'Action' monad.
liftAction :: LabelState l p s => M.Action (UnsafeLIO l p s) a -> Action l p s a
liftAction = Action . lift . LIOAction

--
-- Cursor
--

-- | A labeled cursor. The cursor is labeled with the join of the
-- database and collection it reads from.
data Cursor l = Cursor { curLabel   :: l                  -- ^ Cursorlabel
                       , curIntern  :: M.Cursor           -- ^ Actual cursor
                       , curProject :: M.Projector        -- ^ Projector from query
                       , curPolicy  :: CollectionPolicy l -- ^ Collection policy
                       } 

