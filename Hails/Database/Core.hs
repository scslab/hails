{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- |


-}

module Hails.Database.Core (
   -- * Collection
     CollectionName
   , CollectionSet
   , Collection
   , collection, collectionP
     -- * Database
   , DatabaseName
   , Database
   , database
   , associateCollection, associateCollectionP
     -- * Policies
   , CollectionPolicy(..)
   , FieldPolicy(..)
   , isSearchableField
   , searchableFields
   ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import           LIO
import           LIO.DCLabel

import           Hails.Data.Hson
import           Hails.PolicyModule.TCB
import           Hails.Database.TCB

--
-- Collection
--

-- | Create a 'Collection' given a name, label, clearance, and policy.
-- The supplied collection label and clearance must be above the current
-- label and below the current clearance as enforced by 'guardAlloc'.
collection :: MonadDC m
           => CollectionName  -- ^ Collection name
           -> DCLabel         -- ^ Collection label
           -> DCLabel         -- ^ Collection clearance
           -> CollectionPolicy-- ^ Collection policy
           -> m Collection
collection = collectionP noPriv

-- | Same as 'collection', but uses privileges when comparing the
-- supplied collection label and clearance with the current label and
-- clearance.
collectionP :: MonadDC m 
            => DCPriv           -- ^ Privileges
            -> CollectionName   -- ^ Collection name
            -> DCLabel          -- ^ Collection label
            -> DCLabel          -- ^ Collection clearance
            -> CollectionPolicy -- ^ Collection policy
            -> m Collection
collectionP p n l c pol = do
  guardAllocP p l
  guardAllocP p c
  return $ collectionTCB n l c pol

--
-- Database
--

-- | Given a policy module configuration, the label of the database and
-- label on collection set create the policy module's database.  Note
-- that the label of the database and collection set must be above the
-- current label (modulo the policy module's privileges) and below the
-- current clearance as imposed by 'guardAllocP'.
database :: MonadDC m 
         => PolicyModuleConf  -- ^ Policy module configuration
         -> DCLabel           -- ^ Label of database
         -> DCLabel           -- ^ Label of collection set
         -> m Database
database conf ldb lcoll = do
  guardAllocP p ldb
  cs <- labelP p lcoll Set.empty
  return $ databaseTCB n ldb cs
   where n = policyModuleDBName conf
         p = policyModulePriv conf

-- | Given a newly created collection and an existing database,
-- associate the collection with the database. To do so, the current
-- computation must be able to modify the database's collection set.
-- Specifically, the current label must equal to the collection set's
-- label as specified by the policy module. This is enforced by
-- 'guardWrite'.
associateCollection :: MonadDC m
                    => Collection  -- ^ New collection
                    -> Database    -- ^ Existing database
                    -> m Database  -- ^ New database
associateCollection = associateCollectionP noPriv

-- | Same as 'associateCollection', but uses privileges when
-- performing label comparisons and raising the current label.
associateCollectionP :: MonadDC m
                     => DCPriv      -- ^ Privileges
                     -> Collection  -- ^ New collection
                     -> Database    -- ^ Existing database
                     -> m Database  -- ^ New database
associateCollectionP p c db = do
  guardWriteP p $ labelOf (databaseCollections db)
  return $ associateCollectionTCB c db

--
-- Policies
--

-- | Returns 'True' if the field policy is a 'SearchableField'.
isSearchableField :: FieldPolicy -> Bool
isSearchableField SearchableField = True
isSearchableField _ = False

-- | Get the list of names corresponding to 'SearchableField's.
searchableFields :: CollectionPolicy -> [FieldName]
searchableFields policy =
  Map.keys $ Map.filter isSearchableField fps
  where fps = fieldLabelPolicies policy
