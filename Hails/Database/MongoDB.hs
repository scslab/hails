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
                              , database, databaseP
                              , assocCollection, assocCollectionP
                              , RawPolicy(..)
                              , PolicyError(..)
                              , Action, getDatabase
                              , Query(..)
                              , Cursor
                              -- * Query
                              , insert, insert_, insertP, insertP_
                              , defaultQuery
                              , findP
                              , next, nextP
                              -- * Misc
                              , Failure
                              ) where

import Hails.Database.MongoDB.TCB.Access
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Query
import Hails.Data.LBson
import Database.MongoDB.Query (Failure)
