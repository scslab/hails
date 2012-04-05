{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hails.Database.MongoDB.Structured ( DCRecord(..)
                                         , DCLabeledRecord(..)
                                         ) where

import LIO
import LIO.TCB (labelTCB, unlabelTCB)
import LIO.DCLabel

import Hails.Database
import Hails.Database.MongoDB

import Data.Monoid (mappend)
import Control.Monad (liftM)

-- | Class for converting from \"structured\" records to documents
-- (and vice versa).
class DCRecord a where
  -- | Convert a document to a record
  fromDocument :: Monad m => Document DCLabel -> m a
  -- | Convert a record to a document
  toDocument :: a -> Document DCLabel
  -- | Get the collection name for the record
  collectionName :: a -> CollectionName
  -- | Find an object with mathing value for the given key. If the
  -- object does exist but cannot be read (above clearance), this
  -- returns 'Nothing'.
  findBy :: (Val DCLabel v, DatabasePolicy p)
         => p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Find an object with given query
  findWhere :: (DatabasePolicy p)
            => p -> Query DCLabel -> DC (Maybe a)
  -- | Insert a record into the database
  insertRecord :: (DatabasePolicy p)
               => p -> a -> DC (Either Failure (Value DCLabel))
  -- | Perform all the checks and taints as if inserting document, but
  -- does not insert.
  insertRecordGuard :: (DatabasePolicy p)
               => p -> a -> DC (Either Failure ())
  -- | Insert a record into the database
  saveRecord :: (DatabasePolicy p)
             => p -> a -> DC (Either Failure ())
  -- | Delete a record from the database given a matching value for
  -- given key. The deleted record is returned.
  deleteBy :: (Val DCLabel v, DatabasePolicy p)
           => p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Delete an object matching the given query.
  -- The deleted record is returned.
  deleteWhere :: (DatabasePolicy p)
              => p -> Selection DCLabel -> DC (Maybe a)
  -- | Same as 'findBy', but using explicit privileges.
  findByP :: (Val DCLabel v, DatabasePolicy p)
          => DCPrivTCB -> p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Same as 'findWhere', but using explicit privileges.
  findWhereP :: (DatabasePolicy p)
            => DCPrivTCB -> p -> Query DCLabel -> DC (Maybe a)
  -- | Same as 'insertRecord', but using explicit privileges.
  insertRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> a -> DC (Either Failure (Value DCLabel))
  -- | Same as 'insertRecordGuard', but using explicit privileges.
  insertRecordGuardP :: (DatabasePolicy p)
               => DCPrivTCB -> p -> a -> DC (Either Failure ())
  -- | Same as 'saveRecord', but using explicit privileges.
  saveRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> a -> DC (Either Failure ())
  -- | Same as 'deleteBy', but using explicit privileges.
  deleteByP :: (Val DCLabel v, DatabasePolicy p)
      => DCPrivTCB -> p -> CollectionName -> Key -> v -> DC (Maybe a)
  -- | Same as 'deleteWhere', but using explicit privileges.
  deleteWhereP :: (DatabasePolicy p)
               => DCPrivTCB -> p -> Selection DCLabel -> DC (Maybe a)

  --
  -- Default definitions
  --

  --
  findBy = findByP noPrivs
  --
  findWhere = findWhereP noPrivs
  --
  insertRecord = insertRecordP noPrivs
  --
  insertRecordGuard = insertRecordGuardP noPrivs
  --
  saveRecord = saveRecordP noPrivs
  --
  deleteBy = deleteByP noPrivs
  --
  deleteWhere = deleteWhereP noPrivs
  --
  insertRecordP p policy record = do
    let colName = collectionName record
    p' <- getPrivileges
    withDB policy $ insertP (p' `mappend` p)  colName $ toDocument record
  --
  insertRecordGuardP p policy record = do
    let colName = collectionName record
    p' <- getPrivileges
    withDB policy $ insertGuardP (p' `mappend` p) colName $ toDocument record
  --
  saveRecordP p policy record = do
    let colName = collectionName record
    p' <- getPrivileges
    withDB policy $ saveP (p' `mappend` p) colName $ toDocument record
  --
  findByP p policy colName k v = 
    findWhereP p policy (select [k =: v] colName)
  --
  findWhereP p policy query  = do
    result <- withDB policy $ findOneP p query
    c <- getClearance
    case result of
      Right (Just r) | labelOf r `leq` c -> fromDocument `liftM` unlabelP p r
      _ -> return Nothing
  --
  deleteByP p policy colName k v = 
    deleteWhereP p policy (select [k =: v] colName)
  --
  deleteWhereP p policy sel = do
    -- Find with only supplied privileges
    mdoc <- findWhereP p policy $ select (selector sel) (coll sel)
    -- User underlying privileges as well:
    p' <- getPrivileges
    res <- withDB policy $ deleteOneP (p' `mappend` p) sel
    case res of
      Right _ -> return mdoc
      _ -> return Nothing
  --

-- | Class for inserting and saving labeled records.
class DCRecord a => DCLabeledRecord a where
  -- | Insert a labeled record into the database
  insertLabeledRecord :: (DatabasePolicy p)
               => p -> DCLabeled a -> DC (Either Failure (Value DCLabel))
  -- | Perform all the checks and taints as if inserting document, but
  -- does not insert.
  insertLabeledRecordGuard :: (DatabasePolicy p)
               => p -> DCLabeled a -> DC (Either Failure ())
  -- | Insert a labeled record into the database
  saveLabeledRecord :: (DatabasePolicy p)
             => p -> DCLabeled a -> DC (Either Failure ())
  -- | Same as 'insertLabeledRecord', but using explicit privileges.
  insertLabeledRecordP :: (DatabasePolicy p)
    => DCPrivTCB -> p -> DCLabeled a -> DC (Either Failure (Value DCLabel))
  -- | Same as 'insertLabeledRecordGuard', but using explicit privileges.
  insertLabeledRecordGuardP :: (DatabasePolicy p)
               => DCPrivTCB -> p -> DCLabeled a -> DC (Either Failure ())
  -- | Same as 'saveLabeledRecord', but using explicit privileges.
  saveLabeledRecordP :: (DatabasePolicy p)
              => DCPrivTCB -> p -> DCLabeled a -> DC (Either Failure ())

  --
  -- Default definitions
  --

  --
  insertLabeledRecord = insertLabeledRecordP noPrivs
  --
  insertLabeledRecordGuard = insertLabeledRecordGuardP noPrivs
  --
  saveLabeledRecord = saveLabeledRecordP noPrivs
  --
  insertLabeledRecordP p policy lrecord = do
    let colName = collectionName (forceType lrecord)
    p' <- getPrivileges
    withDB policy $ insertP (p' `mappend` p)  colName $ toDocumentTCB lrecord
  --
  insertLabeledRecordGuardP p policy lrecord = do
    let colName = collectionName (forceType lrecord)
    p' <- getPrivileges
    withDB policy $ insertGuardP (p' `mappend` p) colName $ toDocumentTCB lrecord
  --
  saveLabeledRecordP p policy lrecord = do
    let colName = collectionName (forceType lrecord)
    p' <- getPrivileges
    withDB policy $ saveP (p' `mappend` p) colName $ toDocumentTCB lrecord
  --


--
-- Misc helpers
--

-- | Get the type of a 'DCLabeled' value
forceType :: DCLabeled a -> a
forceType = undefined

-- | Same as 'toDocument' but for labeled records.
toDocumentTCB :: DCRecord a => DCLabeled a -> DCLabeled (Document DCLabel)
toDocumentTCB lr = let r = unlabelTCB lr
                   in labelTCB (labelOf lr) $ toDocument r
