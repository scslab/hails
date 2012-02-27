{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Hails.Database.MongoDB.TCB.Query ( Insert(..)
                                        , findP
                                        , findOneP
                                        , next, nextP
                                        ) where

import Hails.Database.MongoDB.TCB.Access
import Hails.Database.MongoDB.TCB.Types

import LIO
import LIO.TCB


import Data.Functor ((<$>))
import Data.Serialize (Serialize)
import qualified Data.Map as Map
import Hails.Data.LBson.TCB hiding (lookup)
import qualified Database.MongoDB as M

import Control.Monad.Reader hiding (liftIO)
import qualified Control.Exception as E

--
-- Write 
--

-- | Class used to overload inserting labeled and unlabeled documents
-- into a collection. Only the definition for @inserP@ is needed.
class Label l => Insert l doc where
  -- | Insert document into collection and return its @_id@ value,
  -- which is created automatically if not supplied.
  insert :: (LabelState l p s, Serialize l)
         => CollectionName
         -> doc
         -> Action l p s M.Value
  insert = insertP noPrivs
  -- | Same as 'insert' except it does not return @_id@
  insert_ :: (LabelState l p s, Serialize l)
          => CollectionName
          -> doc
          -> Action l p s ()
  insert_ c d = insert c d >> return ()
  -- | Same as 'insert', but uses privileges when applying the
  -- collection policies, and doing label comparisons.
  insertP :: (LabelState l p s, Serialize l)
          => p 
          -> CollectionName
          -> doc
          -> Action l p s M.Value
  -- | Same as 'insertP' except it does not return @_id@
  insertP_ :: (LabelState l p s, Serialize l)
           => p 
           -> CollectionName
           -> doc
           -> Action l p s ()
  insertP_ p c d = insertP p c d >> return ()


instance Label l => Insert l (Document l) where
  insertP p' colName doc = do
    db <- getDatabase
    col <- liftLIO $  withCombinedPrivs p' $ \p -> do
      -- Check that we can read collection names associated with database:
      colMap <- unlabelP p $ dbColPolicies db
      maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
    let clearance = colClear col
    ldoc <- liftLIO $ withCombinedPrivs p' $ \p -> do
              -- Check that we can write to database:
              wguardP p (dbLabel db)
              -- Check that we can write to collection:
              wguardP p (colLabel col)
              -- Apply policies (data should not be labeled with a label
              -- that is above the collection clearance):
              ldoc <- withClearance clearance $ applyRawPolicyP p col doc
              -- Check that 'Labeled' values have labels below clearnce:
              guardLabeledVals (unlabelTCB ldoc) clearance
              -- Policies applied & labels are below clearance:
              return ldoc
    let bsonDoc = toBsonDoc . unlabelTCB $ ldoc
    liftAction $ M.useDb (dbIntern db) $ M.insert colName bsonDoc
      where guardLabeledVals []            _ = return ()
            guardLabeledVals ((_ := v):ds) c = do
              case v of
                (LabeledVal lv) -> unless (labelOf lv `leq` c) $
                                     throwIO LerrClearance
                _               -> return ()
              guardLabeledVals ds c

instance Label l => Insert l (Labeled l (Document l)) where
  insertP p' colName ldoc = do
    db <- getDatabase
    liftLIO $ withCombinedPrivs p' $ \p -> do
      -- Check that we can read collection names associated with database:
      colMap <- unlabelP p $ dbColPolicies db
      col <- maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
      -- Check that we can write to database:
      wguardP p (dbLabel db)
      -- Check that we can write to collection:
      wguardP p (colLabel col)
      -- Check that the labels of all labeled values are below
      -- clearance, check that 'PolicyLabeled' values match, and check that
      -- the label of the document is policy-generated and below clearance:
      guardAll col
    let bsonDoc = toBsonDoc . unlabelTCB $ ldoc
    liftAction $ M.useDb (dbIntern db) $ M.insert colName bsonDoc
      where doc = unlabelTCB ldoc
            --
            guardAll col = do
              -- Apply policy to document:
              ldoc' <- applyRawPolicyTCB col doc
              -- Check that document labels match:
              unless (labelOf ldoc' == labelOf ldoc) $ throwIO PolicyViolation
              -- Check that the document label is below collection clerance:
              unless (labelOf ldoc `leq` colClear col) $ throwIO LerrClearance
              -- Check that fields match and are below collection clearance.
              -- Fields are protected by document label, so if an
              -- exception is thrown it should have this label.
              guardFields (unlabelTCB ldoc') doc
            --
            guardFields []               []               = return ()
            guardFields ((k0 := v0):ds0) ((k1 := v1):ds1) = do
              unless (k0 == k1 && v0 `eq` v1) $ throwViolation
              guardFields ds0 ds1
            guardFields _                _                = throwViolation
            --
            eq (BsonVal v1)           (BsonVal v2)           = v1 == v2
            eq (LabeledVal lv1)       (LabeledVal lv2)       = lv1 `eqL` lv2
            eq (PolicyLabeledVal lv1) (PolicyLabeledVal lv2) = lv1 `eqPL` lv2
            eq _                      _                      = False
            --
            eqL lv1 lv2 = (labelOf lv1 == labelOf lv2) &&
                          (unlabelTCB lv1 == unlabelTCB lv2)
            --
            eqPL (PL lv1) (PL lv2) = lv1 `eqL` lv2
            eqPL _        _        = False
            --
            throwViolation = ioTCB $ E.throwIO $ LabeledExceptionTCB
                                (labelOf ldoc) (E.toException PolicyViolation)


validateSearchableClause :: M.Document -> CollectionPolicy l -> Bool
validateSearchableClause doc policy = and (map isSearchable doc)
  where isSearchable (key M.:= value) =
          case lookup key fieldPolicies of
            Just SearchableField -> True
            _ -> False
        fieldPolicies = rawFieldPolicies . colPolicy $ policy

--
-- Read
--

-- | Fetch documents satisfying query. A labeled 'Cursor' is returned,
-- which can be used to retrieve the actual 'Document's.
findP :: (LabelState l p s)
    => p -> Query -> Action l p s (Cursor l)
findP p' q = do
  db <- getDatabase
  let slct = selection $ q
  let colName = M.coll slct
  col <- liftLIO $  withCombinedPrivs p' $ \p -> do
    -- Check that we can read collection names associated with database:
    colMap <- unlabelP p $ dbColPolicies db
    maybe (throwIO NoColPolicy) return $ Map.lookup colName colMap
  unless ((validateSearchableClause (M.selector slct) col) &&
          (validateSearchableClause (M.sort q) col) &&
          (validateSearchableClause (M.hint q) col)) $
            liftIO $ throwIO InvalidFieldPolicyType
  cur <- liftAction $ M.useDb (dbIntern db) $ M.find (q {project = []})
  return $ Cursor { curLabel  = (colLabel col) `lub` (dbLabel db)
                  , curIntern = cur 
                  , curProject = project q
                  , curPolicy = col }

-- | Fetch the first document satisfying query, or @Nothing@ if not
-- documents matched the query.
findOneP :: (LabelState l p s, Serialize l)
         => p -> Query -> Action l p s (Maybe (LabeledDocument l))
findOneP p q = findP p q >>= nextP p

-- | Return next document in query result, or @Nothing@ if finished.
-- The current label is raised to join of the current label and
-- 'Cursor' label. The document is labeled according to the
-- underlying 'Collection'\'s policies.
next :: (LabelState l p s, Serialize l)
     => Cursor l
     -> Action l p s (Maybe (LabeledDocument l))
next = nextP noPrivs

-- | Same as 'simpleNext', but usess privileges raising the current label.
nextP :: (LabelState l p s, Serialize l)
      => p
      -> Cursor l
      -> Action l p s (Maybe (LabeledDocument l))
nextP p' cur = do
  -- Rause current label, can read from DB+collection:
  liftLIO $ withCombinedPrivs p' $ \p -> taintP p (curLabel cur)
  md <- fromBsonDoc' <$> (liftAction $ M.next (curIntern cur))
  case md of
    Nothing -> return Nothing
    Just d -> Just <$> (liftLIO $ (fmap) applyProjection
                                $ applyRawPolicyTCB (curPolicy cur) d)
    where fromBsonDoc' = maybe Nothing fromBsonDocStrict
          applyProjection doc = if (length $ curProject cur) == 0
            then doc
            else
              let unsafeDoc = unlabelTCB doc
              in labelTCB (labelOf doc) $
                filter inProjection unsafeDoc
          inProjection (k := _) = case M.look k $ curProject cur of
            Just (M.Int32 1) -> True
            _ -> False

