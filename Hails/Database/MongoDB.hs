{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Hails.Database.MongoDB ( module Hails.Data.LBson
                              -- * Types
                              , CollectionName
                              , CollectionPolicy
                              , Collection
                              , CollectionMap
                              , collection, collectionP
                              , DatabaseName
                              , Database
                              , assocCollection, assocCollectionP
                              , RawPolicy(..)
                              , FieldPolicy(..)
                              , isSearchableField
                              , PolicyError(..)
                              , Action, getDatabase
                              , Selection(..)
                              , Query(..)
                              , Cursor
                              , DBConf
                              , DCAction
                              , dcAccess
                              , labelDatabase
                              , DatabasePolicy(..)
                              , PolicyGroup(..)
                              , relabelGroupsP, relabelGroupsSafe
                              , PrivilegeGrantGate(..)
                              , withLabel
                              , gateToLabeled
                              -- * Query
                              , insert, insert_
                              , insertP, insertP_
                              , save, saveP
                              , deleteOne, deleteOneP
                              , find, findP
                              , findOne, findOneP
                              , next, nextP
                              , select
                              -- * Misc
                              , Failure
                              , labeledDocI
                              ) where

import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Query
import Hails.Database.MongoDB.TCB.DCAccess
import Hails.Data.LBson hiding (sort, find)
import Hails.Database.MongoDB.TCB.Convert

