{-# LANGUAGE Trustworthy #-}

{- |

This module export the definition of the Hails monad, 'HailsAction',
which is a 'DC' monad with access to a database system. Both policy
modules and apps must be actions of a 'MonadHails' monad -- this
guarantees that they must abide by information flow control rules,
while having access to a database system.  This module exports a safe
subset of "Hails.Databaes.Access.TCB".


-}

module Hails.Database.Access(
  -- * Hails monad
    HailsAction, runHailsAction
  , MonadHails
  -- * Database system configuration
  , Pipe, AccessMode(..), master, slaveOk
  -- * Exception thrown by failed database actions
  , Failure(..)
  ) where

import Hails.Database.Access.TCB
