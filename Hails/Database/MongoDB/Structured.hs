{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hails.Database.MongoDB.Structured ( DCRecord(..)
                                         , DCLabeledRecord(..)
                                         , MkToLabeledDocument(..)
                                         , toDocumentP
                                         ) where

import LIO
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
      Right (Just r) | leqp p (labelOf r) c -> fromDocument `liftM` unlabelP p r
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
  insertLabeledRecord :: (MkToLabeledDocument p)
               => p -> DCLabeled a -> DC (Either Failure (Value DCLabel))
  -- | Insert a labeled record into the database
  saveLabeledRecord :: (MkToLabeledDocument p)
             => p -> DCLabeled a -> DC (Either Failure ())
  -- | Same as 'insertLabeledRecord', but using explicit privileges.
  insertLabeledRecordP :: (MkToLabeledDocument p)
    => DCPrivTCB -> p -> DCLabeled a -> DC (Either Failure (Value DCLabel))
  -- | Same as 'saveLabeledRecord', but using explicit privileges.
  saveLabeledRecordP :: (MkToLabeledDocument p)
              => DCPrivTCB -> p -> DCLabeled a -> DC (Either Failure ())

  --
  -- Default definitions for insert/save
  --

  --
  insertLabeledRecord = insertLabeledRecordP noPrivs
  --
  saveLabeledRecord = saveLabeledRecordP noPrivs
  --
  insertLabeledRecordP p policy lrecord = do
    let colName = collectionName (forceType lrecord)
    p' <- getPrivileges
    ldoc <- mkToLabeledDocument policy lrecord
    withDB policy $ insertP (p' `mappend` p)  colName  ldoc
  --
  saveLabeledRecordP p policy lrecord = do
    let colName = collectionName (forceType lrecord)
    p' <- getPrivileges
    ldoc <- mkToLabeledDocument policy lrecord
    withDB policy $ saveP (p' `mappend` p) colName ldoc
  --

-- | Classe used by a database policy to translate a labeled record to
-- a labeled document.
class DatabasePolicy p => MkToLabeledDocument p where
  -- | Given a policy, return a function that can be used to translate
  -- labeled records to labeled documents. It is recommended to simply
  -- create the instance by defining @mkToDocumentP@ as:
  --
  -- > mkToDocumentP (Policy ... priv ..) = toDocumentP priv
  --
  mkToLabeledDocument :: DCRecord a
                      => p
                      -> DCLabeled a
                      -> DC (DCLabeled (Document DCLabel))

-- | Same as 'toDocument' but for uses the policy's privileges to
-- convert a labeled record to a labeled  document.
toDocumentP :: DCRecord a
            => DCPrivTCB -> DCLabeled a -> DC (DCLabeled (Document DCLabel))
toDocumentP privs lr = do
  lcur <- getLabel
  let lres = lostar privs lcur (labelOf lr)
  r <- unlabelP privs lr
  labelP privs lres $ toDocument r


--
-- Misc helpers
--

-- | Get the type of a 'DCLabeled' value
forceType :: DCLabeled a -> a
forceType = undefined
