{-# LANGUAGE Trustworthy #-}
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
   PolicyModule(..)
 -- * Helper functions
 , labelDatabaseP
 , associateCollectionsP 
 ) where


import           Data.Typeable

import           LIO.DCLabel
import           Hails.Database.Core

-- | A policy module is specified as an instance of the @PolicyModule@
-- class. The role of this class is to define an entry point for
-- policy modules. The policy module author should set up the database
-- labels and define all the database collections in 'initPolicyModule'.
-- It is these collections and corresponding policies that apps and
-- other policy modules use when interacting with the policy module's
-- database.
--
-- The Hails runtime system relies on the policy module's type @pm@
-- to load the corresponding 'initPolicyModule' when some code
-- \"invokes\" the policy module. Hence, a value of this type may be
-- used by apps and other policy modules as a \"handle\" to the policy
-- module database. Additinoally, and more interestingly, the policy
-- module can use a type for which it does not export the value
-- constructor to hide the Hails supplied privilege. In doing so,
-- policy module code can request a \"handle\" to itself and use the
-- privileges to perform otherwise non-permitted databse actions.
-- By not exporting the value constructor, other code is restricted to
-- the policies imposed in 'initPolicyModule'. The example below show
-- a use case:
--
-- >  import LIO
-- >  import LIO.DCLabel
-- >  import Data.Typeable
-- >  import Hails.PolicyModule
-- >  
-- >  -- | Handle to policy module, not exporting @MyPolicyModuleTCB@
-- >  data MyPolicyModule = MyPolicyModuleTCB DCPriv deriving Typeable
-- >  
-- >  instance PolicyModule MyPolicyModule where
-- >    initPolicyModule priv = do
-- >          -- Get the policy module principal:
-- >      let this = privDesc priv
-- >          -- Create label:
-- >          l    = dcLabel dcTrue -- Everybody can read
-- >                         this   -- Only policy module can modify
-- >      -- Label database and collection map:
-- >      labelDatabaseP priv l l
-- >      -- Associate collections with database
-- >      associateCollectionsP priv [ {- ... my collections ... -} ]
-- >      -- Return "handle" to this policy jodule
-- >      return (MyPolicyModuleTCB priv)
--
-- TODO: add doc on \"handle\"
class Typeable pm => PolicyModule pm where
  -- | Entry point for policy module.
  initPolicyModule :: DCPriv -> DBAction pm

-- | This is the first action that any policy module should execute.
-- Given the policy module's privilges, label for the databse, and
-- label for the collection map @labelDatabaseP@ accordingly sets the
-- labels on the policy module's database.
labelDatabaseP :: DCPriv    -- ^ Policy module privilges
               -> DCLabel   -- ^ Database label
               -> DCLabel   -- ^ Collections label
               -> DBAction ()
labelDatabaseP p ldb lcol = do
  setDatabaseLabelP p ldb
  setCollectionsLabelP p lcol

-- | Given the policy module's privileges and list of collections,
-- associate the collections with the policy module's database.
associateCollectionsP :: DCPriv         -- ^ Policy module privileges
                      -> [Collection]   -- ^ List of collections
                      -> DBAction ()
associateCollectionsP p cs = mapM_ (associateCollectionP p) cs

