{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- |

A /policy module/ is a library with access to the privileges of a
dedicated principal (conceptually, the author of the library) and
associated with a dedicated database ('PolicyModuleConf'). The job of
the policy module is to specify what sort of data may be stored in
this database, and what access-control policies should be applied to
it. However, because Hails uses information flow control (IFC) to enforce
policies, a policy specified by a policy module on a piece of data is
enforce even when an app gets a hold of the data.

IFC lets apps and policy modules productively use other policy modules
despite mutual distrust.  Moreover, IFC prevents malicious apps from
violating any of the policies specified by a policy module. As a
consequence, users need not place as much trust in apps. Rather, they
need to trust or verify the policies specified by policy modules.

This moule exports the class which every policy module must be an
instance of. Though simple, the class allows a policy module to create
a database and relevant collections with a set of policies.

-}


module Hails.PolicyModule (
 -- * Policy module configurations
    PolicyModuleConf
 , policyModuleDBName
 , policyModulePriv
 -- * Policy module class
 , PolicyModule(..)
 ) where

import           Hails.PolicyModule.TCB
import           Hails.Database.Access
import           Hails.Database.Core

-- | A policy module is specified as an instance of the @PolicyModule@
-- class. The role of this class is two-fold.
--
-- * Each policy module may create a database and collection by
--   providing a definintion of 'createPolicyModule'
--
-- * An application may interact with a policy module's database with
--   'getPolicyModuleDB'.
class PolicyModule pm where
  -- | Each policy module is provided a unique policy module configuration
  -- value ('PolicyModuleConf') which contains the policy module's underlying
  -- database name and privilege. This value should be treated as
  -- sensitive by the policy module code since the value encapsulates
  -- the policy module's privileges.
  --
  -- Example use case:
  --
  -- > module MyPolicyModule ( MyPolicyModule ) where
  -- >
  -- > data MyPolicyModule = MyPolicyModuleTCB DCPriv Database
  -- >   deriving Typeable
  -- >
  -- > instance PolicyModule MyPolicyModule
  -- >   createPolicyModule conf = do
  -- >     let myPrivs = policyModulePriv conf
  -- >     ...
  -- >     db <- {- create database -}
  -- >     ...
  -- >     return (MyPolicyModuleTCB priv db)
  -- >
  -- >   getPolicyModuleDB (MyPolicyModuleTCB _ db) = db
  -- 
  -- Within this module, the policy module's privilges are available
  -- since the value constructor 'MyPolicyModuleTCB' is visible.
  -- However, application code cannot access these privileges.
  createPolicyModule :: PolicyModuleConf -> HailsAction pm

  -- | Retrieve the policy module's database. To restrict access to
  -- the database \"handle\" should be labeled
  getPolicyModuleDB :: pm -> Database
