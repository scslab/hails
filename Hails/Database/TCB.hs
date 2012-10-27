{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             StandaloneDeriving,
             DeriveDataTypeable,
             TypeSynonymInstances #-}

{- |

This module exports the basic database types and constructors.
See "Hails.Database" for a description of the Hails database system.

-}

module Hails.Database.TCB (
  -- * Collection
    CollectionName
  , CollectionSet
  , Collection(..)
  , collectionTCB
    -- * Database
  , DatabaseName
  , Database(..)
  -- * Policies
  , CollectionPolicy(..)
  , FieldPolicy(..)
  -- * Hails DB monad
  , DBAction(..), DBActionState(..)
  , getActionStateTCB
  , putActionStateTCB
  , updateActionStateTCB
  , makeDBActionStateTCB 
  , setDatabaseLabelTCB
  , setCollectionSetLabelTCB 
  , associateCollectionTCB 
  -- ** Database system configuration
  , Pipe, AccessMode(..), master, slaveOk
  -- ** Exception thrown by failed database actions
  , DBError(..)
  -- ** Lifting "Database.MongoDB" actions
  , execMongoActionTCB 
  ) where

import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import           Data.Typeable

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Control.Exception

import qualified Database.MongoDB as Mongo
import           Database.MongoDB.Connection ( Pipe )
import           Database.MongoDB.Query ( AccessMode(..)
                                        , master
                                        , slaveOk
                                        , Failure(..)
                                        )

import           LIO
import           LIO.TCB (rethrowIoTCB)
import           LIO.Labeled.TCB (labelTCB, unlabelTCB)
import           LIO.DCLabel

import           Hails.Data.Hson

--
-- Collections
--

-- | The name of a collection.
type CollectionName = Text


-- | A @Collection@ is a MongoDB collection name with an associated
-- label, clearance and labeling policy. Access to the collection is
-- restricted according to the collection label. Data inserted-to and
-- retrieved-from the collection will be labeled according to the
-- collection policy, with the guarantee that no data more sensitive than
-- the collection clearance can be inserted into the collection.
data Collection = CollectionTCB { colName :: CollectionName
                                -- ^ Collection name
                                , colLabel :: DCLabel
                                -- ^ Collection label
                                , colClearance :: DCLabel
                                -- ^ Collection clearance
                                , colPolicy :: CollectionPolicy
                                -- ^ Collection labeling policies
                                }

instance Eq Collection where
  c1 == c2 = colName c1 == colName c2

instance Ord Collection where
  c1 <= c2 = colName c1 <= colName c2

-- | Create a 'Collection', ignoring any IFC restrictions.
collectionTCB :: CollectionName   -- ^ Collection name
              -> DCLabel          -- ^ Collection label
              -> DCLabel          -- ^ Collection clearance
              -> CollectionPolicy -- ^ Collection policy
              -> Collection
collectionTCB n l c p = CollectionTCB { colName      = n
                                      , colLabel     = l
                                      , colClearance = c
                                      , colPolicy    = p
                                      }

--
-- Policies
--

-- | A collection policy contains the policy for labeling documents
-- ('documentLabelPolicy') at a coarse grained level, and a set of
-- policies for labeling fields of a document ('fieldLabelPolicies').
-- 
-- Specific fields can be associated with a 'FieldPolicy', which
-- allows the policy module to either:
-- 
-- * Explicitly make a field publicly readable to anyone who can
--   access the collection by declaring the field to be a
--   'SearchableField', or
--
-- * Label a field given the full documnet (see 'FieldPolicy').
--
-- Fields that do not have an associated policy are (conceputally)
-- labeled with the document label ('documentLabelPolicy').
-- Similarly, the labels on the label of a policy-labeled field is the
-- document label created with 'documentLabelPolicy'. /Note:/ the
-- label on 'SearchableField's is solely the collection label.
data CollectionPolicy = CollectionPolicy {
      documentLabelPolicy :: HsonDocument -> DCLabel
    -- ^ The label on documents of the collection.
    , fieldLabelPolicies  :: Map FieldName FieldPolicy
    -- ^ The policies associated with specific fields.
  }

-- | A @FieldPolicy@ is a security policy associated with fields.
-- 'SearchabelField' specifies that the field can be referenced in the
-- selection clause of a @Query@, and therefore only the collection label
-- protects such fields. Conversely, 'FieldPolicy' specifies a labeling
-- policy for the field.
data FieldPolicy = SearchableField
                 -- ^ Unlabeled, searchable field.
                 | FieldPolicy (HsonDocument -> DCLabel)
                 -- ^ Policy labeled field.

--
-- Databases
--


-- | The name of a database.
type DatabaseName = Text

-- | A labeled 'Collection' set.
type CollectionSet = DCLabeled (Set Collection)

-- | A @Database@ is a MongoDB database with an associated label and set
-- of collections. The label is used to restrict access to the database.
-- Since collection policies are specified by policy modules, every
-- collection must /always/ be associated with some database (and
-- thereby, policy module); a policy module is /not/ allowed to create a
-- collection (and specify policies on it) in an arbitrary database.  We
-- allow for the existance of a collection to be secrect, and thus
-- protect the set of collections with a label.
data Database = DatabaseTCB { databaseName :: DatabaseName
                              -- ^ Database name
                            , databaseLabel :: DCLabel
                              -- ^ Label of database
                            , databaseCollections :: CollectionSet
                              -- ^ Collections associated with databsae
                            }

--
-- DB monad
--

-- | The database system state threaded within a Hails computation.
data DBActionState = DBActionStateTCB {
    dbActionPipe :: Pipe
    -- ^ Pipe to underlying database system
  , dbActionMode :: AccessMode
    -- ^ Types of reads/write to perform
  , dbActionDB   :: Database
    -- ^ Database computation is currently executing against
  , dbActionPriv :: DCPriv
    -- ^ Privilege of the policy module related to the DB
  }

-- | A @DBAction@ is the monad within which database actions can be
-- executed, and policy modules are defined.  The monad is simply a
-- state monad with 'DC' as monad as the underlying monad with access to
-- a database system configuration ('Pipe', 'AccessMode', and
-- 'Database').  The value constructor is part of the @TCB@ as to
-- disallow untrusted code from modifying the access mode.
newtype DBAction a = DBActionTCB { unDBAction :: StateT DBActionState DC a }
  deriving (Monad, Functor, Applicative)

-- | Get the underlying state.
getActionStateTCB :: DBAction DBActionState
getActionStateTCB = DBActionTCB get

-- | Get the underlying state.
putActionStateTCB :: DBActionState -> DBAction ()
putActionStateTCB = DBActionTCB . put

-- | Update the underlying state using the supplied function.
updateActionStateTCB :: (DBActionState -> DBActionState) -> DBAction ()
updateActionStateTCB f = do
  s <- getActionStateTCB 
  putActionStateTCB $ f s

instance MonadLIO DCLabel DBAction where
  liftLIO = DBActionTCB . lift

-- | Given a policy module's privileges, database name, pipe and access
-- mode create the initial state for a 'DBAction'. The underlying
-- database is labeled with the supplied privileges: both components of
-- the label (secrecy and integrity) are set to the privilege
-- description. In other words, only code that owns the policy module's
-- privileges can modify the database configuration.  Policy modules can
-- use 'setDatabaseLabelP' to change the label of their database, and
-- 'setCollectionMapLabelP' to change the label of the collection map.
makeDBActionStateTCB :: DCPriv
                     -> DatabaseName
                     -> Pipe
                     -> AccessMode
                     -> DBActionState
makeDBActionStateTCB priv dbName pipe mode = 
  DBActionStateTCB { dbActionPipe = pipe
                   , dbActionMode = mode
                   , dbActionDB   = db
                   , dbActionPriv = priv }
    where db = DatabaseTCB { databaseName  = dbName 
                           , databaseLabel = l
                           , databaseCollections = labelTCB l Set.empty }
          l = dcLabel prin prin
          prin = privDesc priv

-- | Set the label of the underlying database to the supplied label,
-- ignoring IFC.
setDatabaseLabelTCB :: DCLabel -> DBAction ()
setDatabaseLabelTCB l = updateActionStateTCB $ \s -> 
 let db = dbActionDB s
 in s { dbActionDB = db { databaseLabel = l } }

-- | Set the label of the underlying database to the supplied label,
-- ignoring IFC.
setCollectionSetLabelTCB :: DCLabel -> DBAction ()
setCollectionSetLabelTCB l = updateActionStateTCB $ \s -> 
 let db = dbActionDB s
     cs = databaseCollections db
     cs' = labelTCB l $! unlabelTCB cs
 in s { dbActionDB = db { databaseCollections = cs' } }

-- | Associate a collection with underlying database, ignoring IFC.
associateCollectionTCB :: Collection -- ^ New collection
                       -> DBAction ()
associateCollectionTCB col = updateActionStateTCB $ \s -> 
 let db = dbActionDB s
 in s { dbActionDB = doUpdate db }
  where doUpdate db = 
          let cs = databaseCollections db
          in  db { databaseCollections = labelTCB (labelOf cs) $
                                         Set.insert col $ unlabelTCB cs }

-- | Lift a mongoDB action into the 'DBAction' monad. This function
-- always executes the action with "Database.MongoDB"\'s @access@. If
-- the database action fails an exception of type 'Failure' is thrown.
execMongoActionTCB :: Mongo.Action IO a -> DBAction a
execMongoActionTCB act = do
  s <- getActionStateTCB
  let pipe = dbActionPipe s
      mode = dbActionMode s
      db   = databaseName . dbActionDB $ s
  liftLIO $ rethrowIoTCB $ do
    res <- Mongo.access pipe mode db act
    case res of
      Left err -> throwIO $ ExecFailure err
      Right v  -> return v


--
-- DB failures
--


-- | Exceptions thrown by invalid database queries.
data DBError = UnknownCollection   -- ^ Collection does not exist
             | UnknownPolicyModule -- ^ Policy module not found
             | ExecFailure Failure -- ^ Execution of action failed
               deriving (Show, Typeable)

instance Exception DBError
