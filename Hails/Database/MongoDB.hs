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
                              , Query(..)
                              , Cursor
                              , DBConf(..)
                              , DCAction
                              , dcAccess
                              , labelDatabase
                              -- * Query
                              , insert, insert_, insertP, insertP_
                              , defaultQuery
                              , findP
                              , findOneP
                              , next, nextP
                              -- * Misc
                              , Failure
                              ) where

import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Query
import Hails.Database.MongoDB.TCB.DCAccess
import Hails.Data.LBson
import Database.MongoDB.Query (Failure)


