{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             StandaloneDeriving,
             DeriveDataTypeable,
             TypeFamilies,
             TypeSynonymInstances #-}
{- |

This module export the definition of the Hails monad, 'HailsAction',
which is a 'DC' monad with access to a database system. Both policy
modules and apps must be actions of a 'MonadHails' monad -- this
guarantees that they must abide by information flow control rules,
while having access to a database system.

Trusted code can lift MongoDB actions into the 'HailsAction' monad
with 'execMongActionTCB'. This function calls "Database.MongoDB"\'s
@access@ function with an undefined database -- hence ported actions
must always use @useDb@.

-}

module Hails.Database.Access.TCB (
  -- * Hails monad
    HailsAction(..), runHailsAction
  , MonadHails
  -- * Database system configuration
  , Pipe, AccessMode(..), master, slaveOk
  -- * Exception thrown by failed database actions
  , Failure(..)
  -- * Lifting "Database.MongoDB" actions
  , execMongoActionTCB 
  ) where

import           Data.Typeable

import           Control.Applicative
import           Control.Monad.Trans.Reader
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Exception

import           Database.MongoDB.Connection ( Pipe )
import           Database.MongoDB.Query ( AccessMode(..)
                                        , master
                                        , slaveOk
                                        , Failure(..)
                                        )
import qualified Database.MongoDB as Mongo

import           LIO.TCB (rethrowIoTCB)
import           LIO.DCLabel

import           Hails.Database.TCB ( Database(..) )

-- | The database system state threaded within a Hails computation.
data HailsActionState = HailsActionState {
    hailsActionPipe :: Pipe
    -- ^ Pipe to underlying database system
  , hailsActionMode :: AccessMode
    -- ^ Types of reads/write to perform
  , hailsActionDB   :: Database
    -- ^ Current database executing computation against
  }

-- | A @DCAction@ is the underlying monad used by all Hails applications
-- and policy modules. It is simply a reader monad with the 'DC' monad as
-- the underlying monad and database system 'Pipe' and 'AccessMode'
-- being the reader state. This type is part of the @TCB@ as to
-- disallow untrusted code from modifying the access mode.
newtype HailsAction a = HailsActionTCB {
           unHailsAction :: ReaderT HailsActionState DC a }
  deriving (Monad, Functor, Applicative)

instance MonadBase DC HailsAction where
  liftBase = HailsActionTCB . liftBase

instance MonadBaseControl DC HailsAction where
  newtype StM HailsAction a = StM { unStM :: HailsAction a }
  liftBaseWith f = liftBase $ f $ return . StM
  restoreM       = unStM

-- | A Hails monad is one for which 'HailsAction' is the underlying monad.
type MonadHails m = MonadBaseControl HailsAction m

-- | Get the database connnection 'Pipe' and 'AccessMode'
getActionState :: HailsAction HailsActionState
getActionState = HailsActionTCB ask

-- | Execute a hails computation given a pre-established connection to
-- a database and the access mode. This function need not be part of
-- the TCB since 'Pipe's can only be created by trusted code.
runHailsAction :: HailsAction a         -- ^ Computation
               -> Pipe                  -- ^ Connection to databse system
               -> AccessMode            -- ^ Type of reads/write
               -> DC a
runHailsAction hact pipe mode = runReaderT (unHailsAction hact) s
    where s = HailsActionState { hailsActionPipe = pipe
                               , hailsActionMode = mode 
                               , hailsActionDB   = db }
          db = error "runHailsAction: HailsActions should always use withDB"

deriving instance Typeable Failure
instance Exception Failure

-- | Lift a mongoDB action into the 'HailsAction' monad. This function
-- always executes the action with "Database.MongoDB"\'s @access@. If
-- the database action fails an exception of type 'Failure' is thrown.
execMongoActionTCB :: Mongo.Action IO a -> HailsAction a
execMongoActionTCB act = do
  s <- getActionState
  let pipe = hailsActionPipe s
      mode = hailsActionMode s
      db   = databaseName . hailsActionDB $ s
  liftBase $ rethrowIoTCB $ do
    res <- Mongo.access pipe mode db act
    case res of
      Left err -> throwIO err
      Right v  -> return v
