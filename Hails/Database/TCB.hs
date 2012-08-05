{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

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
   , databaseTCB 
   , associateCollectionTCB 
     -- * Policies
   , CollectionPolicy(..)
   , FieldPolicy(..)
   ) where

import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)

import           LIO
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

-- | Create a database, ignoring IFC.
databaseTCB :: DatabaseName     -- ^ Databse name
            -> DCLabel          -- ^ Label of database
            -> CollectionSet    -- ^ Associated collections
            -> Database
databaseTCB n l cs = DatabaseTCB { databaseName        = n
                                 , databaseLabel       = l
                                 , databaseCollections = cs }

-- | Associate a collection with a database, returning the update
-- database.
associateCollectionTCB :: Collection -- ^ New collection
                       -> Database   -- ^ Existing database
                       -> Database
associateCollectionTCB col db = 
  let cs = databaseCollections db
  in  db { databaseCollections = labelTCB (labelOf cs) $
                                 Set.insert col $ unlabelTCB cs }
