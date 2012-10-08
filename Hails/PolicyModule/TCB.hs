{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             TypeFamilies #-}

{- |

This module exports a newtype wrapper for 'DBAction' that restricts
certain combinators solely to policy modules. Specifically, this
policy module monad ('PMAction') is used when setting labels,
specifing policies, creating collections, etc. The newtype is used to
restrict such functionality to policy modules; apps cannot and should
not be concerned with specifying data models and policies.

-}


module Hails.PolicyModule.TCB (
   PMAction(..)
 ) where


import           Control.Applicative

import           LIO
import           LIO.DCLabel
import           Hails.Database.Core

-- | A policy module action (@PMAction@) is simply a wrapper for
-- database action ('DBAction'). The wrapper is used to restrict /app/
-- code from specifying policies; only policy module may execute
-- @PMAction@s, and thus create collections, set a label on their
-- databases, etc.
newtype PMAction a = PMActionTCB { unPMActionTCB :: DBAction a }
  deriving (Monad, Functor, Applicative)

instance MonadLIO DCLabel PMAction where
  liftLIO = liftDB . liftLIO

instance MonadDB PMAction where
  liftDB = PMActionTCB
