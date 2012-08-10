{-# LANGUAGE Trustworthy #-}

{- |

This module exports labeled documents and the databse monad
('DBAction'). The database monad is used by apps and policy modules to
execute database actions against a policy module's databse (see
"Hails.PolicyModule"). The Hails database model and interface is
documented in "Hails.Database".

-}

module Hails.Database.Core (
  -- * Collection
    CollectionName
  , CollectionSet
  , Collection, colName, colLabel, colClearance, colPolicy
  -- * Database
  , DatabaseName
  , Database, databaseName, databaseLabel, databaseCollections
  -- * Labeled documents
  , LabeledHsonDocument 
  -- * Hails DB monad
  , DBAction, DBActionState(..)
  , runDBAction, evalDBAction
  , getDatabase, getDatabaseP
  -- ** Database system configuration
  , Pipe, AccessMode(..), master, slaveOk
  -- ** Exception thrown by failed database actions
  , Failure(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans.State

import           LIO
import           LIO.DCLabel

import           Hails.Data.Hson
import           Hails.Database.TCB


--
-- Labeled documents
--
-- | A labeled 'HsonDocument'.
type LabeledHsonDocument = DCLabeled HsonDocument

--
-- DB monad
--

-- | Execute a database action returning the final result and state.
-- In general, code should instead use 'evalDBAction'. This function
-- is primarily used by trusted code to initialize a policy module
-- which may have modified the underlying database.
runDBAction :: DBAction a -> DBActionState -> DC (a, DBActionState)
runDBAction = runStateT . unDBAction

-- | Execute a database action returning the final result.
evalDBAction :: DBAction a -> DBActionState -> DC a
evalDBAction a s = fst `liftM` runDBAction a s

-- | Get the underlying database. Must be able to read from the
-- database as enforced by applying 'taint' to the database label.
-- This is required because the database label protects the
-- label on collections which can be projected given a 'Database'
-- value.
getDatabase :: DBAction Database
getDatabase = getDatabaseP noPriv

-- | Same as 'getDatabase', but uses privileges when raising the
-- current label.
getDatabaseP :: DCPriv -> DBAction Database
getDatabaseP p = do
  db <- dbActionDB `liftM` getActionStateTCB
  taintP p (databaseLabel db)
  return db
