{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeSynonymInstances #-}

{- |

This module exports classes 'DCRecord' and 'DCLabeledRecord' that
provide a way for Hails applications to interact with persistent data
more easily. Specifically, it provides a way to work with Haskell
types as opposed to \"unstructured\" 'Document's.

-}
module Hails.Database.Structured ( DCRecord(..)
                                 , findAll, findAllP
                                 , DCLabeledRecord(..)
                                 ) where

import qualified Data.Map as Map
import           Data.Monoid (mappend)
import           Control.Monad (liftM)
                 
import           LIO
import           LIO.DCLabel
import           LIO.Privs.TCB (mintTCB) 
                 
import           Hails.Data.Hson
import           Hails.PolicyModule
import           Hails.Database.Core
import           Hails.Database.Query
import           Hails.Database.TCB

-- | Class for converting from \"structured\" records to documents
-- (and vice versa). Minimal definition consists of 'toDocument',
-- 'fromDocument', and 'recordCollection'. All database operations
-- performed on the collection defined by 'recordCollection'.
class DCRecord a where
  -- | Convert a document to a record
  fromDocument :: Monad m => Document -> m a
  -- | Convert a record to a document
  toDocument :: a -> Document
  -- | Get the collection name for the record
  recordCollection :: a -> CollectionName
  -- | Find an object with matching value for the given key. If the
  -- object does not exist or cannot be read (its label is above the
  -- clearance), this returns 'Nothing'.
  findBy :: (BsonVal v, MonadDB m)
         => CollectionName -> FieldName -> v -> m (Maybe a)
  -- | Find an object with given query
  findWhere :: MonadDB m => Query -> m (Maybe a)
  -- | Insert a record into the database
  insertRecord :: MonadDB m => a -> m ObjectId
  -- | Update a record in the database
  saveRecord :: MonadDB m => a -> m ()
  -- | Same as 'findBy', but uses privileges. 
  findByP :: (BsonVal v, MonadDB m)
          => DCPriv -> CollectionName -> FieldName -> v -> m (Maybe a)
  -- | Same as 'findWhere', but uses privileges. 
  findWhereP :: MonadDB m => DCPriv -> Query -> m (Maybe a)
  -- | Same as 'insertRecord', but uses privileges. 
  insertRecordP :: MonadDB m => DCPriv -> a -> m ObjectId
  -- | Same as 'saveRecord', but uses privileges. 
  saveRecordP :: MonadDB m => DCPriv -> a -> m ()

  --
  -- Default definitions
  --

  --
  findBy = findByP noPriv
  --
  findWhere = findWhereP noPriv
  --
  insertRecord = insertRecordP noPriv
  --
  saveRecord = saveRecordP noPriv
  --
  insertRecordP p r = liftDB $ do
    insertP p (recordCollection r) $ toDocument r
  --
  saveRecordP p r = liftDB $ do
    saveP p (recordCollection r) $ toDocument r
  --
  findByP p cName k v = 
    findWhereP p (select [k -: v] cName)
  --
  findWhereP p query  = liftDB $ do
    mldoc <- findOneP p query
    c <- getClearance
    case mldoc of
      Just ldoc | canFlowToP p (labelOf ldoc) c ->
                    fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
      _ -> return Nothing
--   --
--   deleteByP p policy colName k v = 
--     deleteWhereP p policy (select [k =: v] colName)
--   --
--   deleteWhereP p policy sel = do
--     -- Find with only supplied privileges
--     mdoc <- findWhereP p policy $ select (selector sel) (coll sel)
--     -- User underlying privileges as well:
--     p' <- getPrivileges
--     res <- withDB policy $ deleteOneP (p' `mappend` p) sel
--     case res of
--       Right _ -> return mdoc
--       _ -> return Nothing
--   --


-- | Find all records that satisfy the query and can be read, subject
-- to the current clearance.
findAll :: (DCRecord a, MonadDB m) => Query -> m [a]
findAll = findAllP noPriv

-- | Same as 'findAll', but uses privileges.
findAllP :: (DCRecord a, MonadDB m)
         => DCPriv -> Query -> m [a]
findAllP p query = liftDB $ do
  cursor <- findP p query
  cursorToRecords cursor []
  where cursorToRecords cur docs = do
          mldoc <- nextP p cur
          case mldoc of
            Just ldoc -> do
              c <- getClearance
              if canFlowToP p (labelOf ldoc) c
                then do md <- fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                        cursorToRecords cur $ maybe docs (:docs) md
                 else cursorToRecords cur docs
            _ -> return $ reverse docs

-- | Class used by a policy module to translate a labeled record to a
-- labeled document. Since the insert and save functions use the
-- policy module\'s privileges, only the policy module should be
-- allowed to create an instance of this class. Thus, we leverage the 
-- fact that the value constructor for a 'PolicyModule' is not exposed
-- to untrusted code and require the policy module to create such a
-- value in 'endorseInstance'.
class (PolicyModule pm, DCRecord a) => DCLabeledRecord pm a | a -> pm where
  -- | Insert a labeled record into the database.
  insertLabeledRecord :: MonadDB m => DCLabeled a -> m ObjectId
  -- | Insert a labeled record into the database
  saveLabeledRecord :: MonadDB m => DCLabeled a -> m ()

  -- | Same as 'insertLabeledRecord', but using explicit privileges.
  insertLabeledRecordP :: MonadDB m => DCPriv -> DCLabeled a -> m ObjectId
  -- | Same as 'saveLabeledRecord', but using explicit privileges.
  saveLabeledRecordP :: MonadDB m => DCPriv -> DCLabeled a -> m ()

  -- | Endorse the implementation of this instance. Note that this is
  -- reduced to WHNF to catch invalid instances that use 'undefined'.
  --
  -- Example implementation:
  --
  -- > endorseInstance _ = MyPolicyModuleTCB {- May leave other values undefined -}
  endorseInstance :: DCLabeled a -> pm

  --
  -- Default definitions for insert/save
  --

  --
  insertLabeledRecord lrec = liftDB $ do
    dbPriv <- dbActionPriv `liftM` getActionStateTCB
    insertLabeledRecordP dbPriv lrec
  --
  saveLabeledRecord lrec = liftDB $ do
    dbPriv <- dbActionPriv `liftM` getActionStateTCB
    saveLabeledRecordP dbPriv lrec
  --
  insertLabeledRecordP p lrec = liftDB $ do
    let cName = recordCollection (forceType lrec)
    ldoc <- liftLIO $ toDocumentP p lrec
    insertP p cName ldoc

  --
  saveLabeledRecordP p lrec = liftDB $ do
    let cName = recordCollection (forceType lrec)
    ldoc <- liftLIO $ toDocumentP p lrec
    saveP p cName ldoc


-- | Uses the policy modules\'s privileges to convert a labeled record
-- to a labeled document, if the policy module created an instance of
-- 'DCLabeledRecord'.
toDocumentP :: (DCLabeledRecord pm a)
            => DCPriv      -- ^ Policy module privileges
            -> DCLabeled a -- ^ Labeled record
            -> DC (DCLabeled Document)
toDocumentP p' lr = do
  -- Fail if not endorsed:
  pmPriv <- getPMPrivTCB (endorseInstance lr)
  let p = p' `mappend` pmPriv
  r <- unlabelP p lr
  lcur <- getLabel
  let lres = partDowngradeP p lcur (labelOf lr)
  labelP p lres $ toDocument r
    where getPMPrivTCB pm = do
            _ <- evaluate pm
            let tn = policyModuleTypeName pm
                f  = mintTCB . dcPrivDesc . fst
            return $ maybe noPriv f $ Map.lookup tn availablePolicyModules

--
-- Misc helpers
--

-- | Get the type of a 'DCLabeled' value
forceType :: DCLabeled a -> a
forceType = undefined

