module Hails.Database.MongoDB.TCB.Types where

import LIO
import qualified Database.MongoDB as M
import Hails.Data.LBson.TCB

--
-- Collections
--

-- | Name of collection
type CollectionName = M.Collection

-- | A collection is a MongoDB collection associated with a
-- label, clearance and labeling policy. The label
-- specifies who can write to a collection (i.e., only priciples whos
-- current label flows to the label of the
-- collection). The clearance limits
-- the sensitivity of the data written to the collection (i.e.,
-- the labels of all data in the collection must flow to the clearance).
data Collection l = Collection { colLabel  :: l
                               -- ^ Collection label
                               , colClear  :: l
                               -- ^ Collection clearance
															 , colDatabase :: DDatabase l
															 -- ^ Database to which collection belongs
                               , colIntern :: CollectionName
                               -- ^ Actual MongoDB collection
                               , colPolicy :: RawPolicy l
                               -- ^ Collection labeling policy
                               }
instance Label l => Show (Collection l) where
  show c = show "Collection "
              ++ show (colIntern c)
              ++ "\t" ++ show (colLabel c)
              ++ "\t" ++ show (colClear c)

--
-- Databases
--


-- | Name of database
type DatabaseName = M.Database

-- | A database has a label, which is used to enforce who can write to
-- the database, and an internal identifier corresponding to the underlying
-- MongoDB database.
data DDatabase l = DDatabase { dbLabel  :: l      -- ^ Label of database
                           , dbIntern :: DatabaseName -- ^ Actual MongoDB 
                           } deriving (Eq, Show)

--
-- Policies 
--


-- | A @RawPolicy@ encodes a document policy, and all
-- field policies. It is required that all fields of type
-- 'PolicyLabled' have a field/column policy -- if using only this
-- low-level interface a runtime-error will occur if this is not
-- satisfied.
data RawPolicy l = RawPolicy {
      rawDocPolicy     :: Document l -> l
    -- ^ A row (document) policy is a function from a 'Document' to a 'Label'.
    , rawFieldPolicies :: [(Key, Document l -> l)]
    -- ^ A column (field) policy is a function from a 'Document' to a
    -- 'Label', for each field of type 'PolicyLabeled'.
  }
