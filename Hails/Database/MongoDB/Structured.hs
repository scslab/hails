{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}

module Hails.Database.MongoDB.Structured ( DCRecord(..) ) where

import LIO
import LIO.DCLabel

import Hails.Database
import Hails.Database.MongoDB

import Data.Monoid (mappend)

-- | Class for converting from \"structured\" records to documents
-- (and vice versa).
class DCRecord a where
  -- | Convert a document to a record
  fromDocument :: Monad m => Document DCLabel -> m a
  -- | Convert a record to a document
  toDocument :: a -> Document DCLabel
  -- | Get the collection name for the record
  collectionName :: a -> String
  -- | Find an object with mathing value for the given key
  findBy :: (Val DCLabel v, DatabasePolicy p)
         => p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Insert a record into the database
  insertRecord :: (DatabasePolicy p)
               => p -> CollectionName -> a -> DC (Either Failure ())
  -- | Insert a record into the database
  saveRecord :: (DatabasePolicy p)
             => p -> CollectionName -> a -> DC (Either Failure ())
  -- | Same as 'findBy', but using explicit privileges.
  findByP :: (Val DCLabel v, DatabasePolicy p)
          => DCPrivTCB -> p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Same as 'insertRecord', but using explicit privileges.
  insertRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> CollectionName -> a -> DC (Either Failure ())
  -- | Same as 'saveRecord', but using explicit privileges.
  saveRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> CollectionName -> a -> DC (Either Failure ())

  --
  -- Default definitions
  --

  --
  findByP priv policy colName key val = do
    result <- withPrivileges noPrivs $ 
                 withDB policy $ findOneP priv $ select [key =: val] colName
    case result of
      Right (Just p) -> unlabelP priv p >>= fromDocument >>= (return . Just )
      _ -> return Nothing
  --
  findBy = findByP noPrivs
  --
  insertRecordP p policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      insertP_ (priv `mappend` p)  colName $ toDocument record
  --
  insertRecord = insertRecordP noPrivs
  --
  saveRecordP p policy colName record = do
    priv <- getPrivileges
    withDB policy $ do
      saveP (priv `mappend` p) colName $ toDocument record
  --
  saveRecord = saveRecordP noPrivs
  --
