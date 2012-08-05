{-# LANGUAGE Unsafe #-}

{- |

This module exports the type for policy module configurations. See
"Hails.PolicyMode" for a description of policy modules and their roles
within Hails.

-}

module Hails.PolicyModule.TCB (
    PolicyModuleConf(..)
  ) where

import           LIO.DCLabel
import           Hails.Database.TCB (DatabaseName)

-- | A policy module configuration is provided to every policy module by
-- the Hails runtime. The configuration contains a unique database name
-- ('policyModuleDBName') and privilege ('policyModulePriv'). The policy
-- module can use the configuration value to create a labeled database,
-- using the provided privileges to restrict access.
data PolicyModuleConf = PolicyModuleConfTCB { policyModuleDBName :: DatabaseName
                                            -- ^ Underlying database name
                                            , policyModulePriv   :: DCPriv
                                            -- ^ Policy module's privileges
                                            } deriving (Show)
