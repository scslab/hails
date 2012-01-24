{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Hails.Database.MongoDB.TCB where

import LIO
import LIO.TCB ( unlabelTCB
               , labelTCB
               , rtioTCB
               , getTCB
               , putTCB
               , setLabelTCB
               , lowerClrTCB
               )
import LIO.MonadCatch
import Hails.Data.LBson.TCB
import Hails.Database.MongoDB.TCB.Types

import Data.Typeable
import qualified Data.List as List

import Data.Maybe
import Data.Serialize (Serialize, encode, decode)
import Data.CompactString.UTF8 (append, isPrefixOf)

import Database.MongoDB.Connection

import qualified Database.MongoDB as M

import qualified Control.Exception as E
import Control.Applicative (Applicative)
import Control.Monad.Error hiding (liftIO)
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.IO.Class as IO

-- | Create a collection given a collection label, clearance, name,
-- and policy. Note that the collection label and clearance must be
-- above the current label and below the current clearance.
collection :: LabelState l p s
           => l               -- ^ Collection label
           -> l               -- ^ Collection clearance
					 -> DDatabase l
           -> CollectionName  -- ^ Collection name
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collection l c db n pol = collectionP noPrivs l c db n pol

-- | Same as 'collection', but uses privileges when comparing the
-- collection label and clearance with the current label and clearance.
collectionP :: LabelState l p s
           => p               -- ^ Privileges
           -> l               -- ^ Collection label
           -> l               -- ^ Collection clearance
					 -> DDatabase l
           -> CollectionName  -- ^ Collection name
           -> RawPolicy l     -- ^ Collection policy
           -> LIO l p s (Collection l)
collectionP p' l c db n pol = withCombinedPrivs p' $ \p -> do
  aguardP p l
  aguardP p c
  return $ Collection { colLabel  = l
                      , colClear  = c
											, colDatabase = db
                      , colIntern = n
                      , colPolicy = pol
                      }

-- | Apply a raw field/column policy to the field corresponding to the
-- key. If the policy has not been specified for this key, the function
-- throws an exception. Similarly, if the policy has already been
-- applied for this key and the label existing label does not match the
-- newly policy-generated label, an exception is thrown.
-- It is required that the label of any 'Labeled' and 'PolicyLabeled'
-- values be below the clearnce of the collection (this is enforced in
-- 'applyRawPolicyP').
applyRawFieldPolicyP :: (LabelState l p s)
                     => p 
                     -> Collection l
                     -> Document l
                     -> Key
                     -> LIO l p s (Field l)
applyRawFieldPolicyP p col doc k = do
  let policies = rawFieldPolicies . colPolicy $ col
  -- Get the 'PolicyLabeled' value corresponding to k:
  plv <- getPolicyLabeledVal
  -- Find policy corresponding to key k:
  f <- maybe (throwIO NoFieldPolicy) return $ List.lookup k policies 
  -- Apply policy, or check matching labels:
  lv <- case plv of
         (PU v)  -> labelP p (f doc) v
         (PL lv) -> do unless (labelOf lv == f doc) $ throwIO PolicyViolation
                       return lv
  -- Return new field, with policy applied value
  return (k := (PolicyLabeledVal . PL $ lv))
    where getPolicyLabeledVal = case look k doc of
            (Just (PolicyLabeledVal x)) -> return  x
            _                           -> throwIO InvalidPolicy

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled'.
applyRawFieldPoliciesP :: (LabelState l p s)
                       => p 
                       -> Collection l
                       -> Document l
                       -> LIO l p s (Document l)
applyRawFieldPoliciesP p col doc = forM doc $ \field@(k := v) ->
  case v of
    (PolicyLabeledVal _) -> applyRawFieldPolicyP p col doc k
    _                    -> return field

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled', and then apply the raw document/row policy. It
-- must be that every labeled value in the document (including the
-- document itself) have a label that is below the clearance of
-- the collection. However, this is not checked by @applyRawPolicyP@.
-- Instead 'insert' (and similar operators) performs this check.
applyRawPolicyP :: (LabelState l p s)
                => p 
                -> Collection l
                -> Document l
                -> LIO l p s (LabeledDocument l)
applyRawPolicyP p' col doc = withCombinedPrivs p' $ \p -> do
  let docP = rawDocPolicy . colPolicy $ col
  -- Apply field/column policies:
  doc' <- applyRawFieldPoliciesP p col doc
  -- Apply document/row policy:
  labelP p (docP doc') doc'

-- | Same as 'applyRawPolicy', but ignores the current label and
-- clearance when applying policies.
applyRawPolicyTCB :: (LabelState l p s)
                  => Collection l
                  -> Document l
                  -> LIO l p s (LabeledDocument l)
applyRawPolicyTCB col doc = do
  -- Save current state:
  s0 <- getTCB
  -- Set state to most permissive label & clearance:
  setLabelTCB lbot
  lowerClrTCB ltop
  -- Apply policy to document:
  ldoc <- applyRawPolicyP noPrivs col doc
  -- Restore state:
  putTCB s0
  return ldoc

--
-- Exceptions
--

-- | Field/column policies are required for every 'PolicyLabled' value
-- in a document.
data PolicyError = NoFieldPolicy   -- ^ Policy for field not specified
                 | InvalidPolicy   -- ^ Policy application invalid
                 | PolicyViolation -- ^ Policy has been violated
  deriving (Typeable)

instance Show PolicyError where
  show NoFieldPolicy   = "NoFieldPolicy: Field policy not found"
  show InvalidPolicy   = "InvalidPolicy: Invalid policy application"
  show PolicyViolation = "PolicyViolation: Policy has been violated"

instance E.Exception PolicyError


--
-- Monad
--

-- | Since it would be a security violation to make 'LIO' an instance
-- of @MonadIO@, we create a Mongo-specific, non-exported,  wrapper for
-- 'LIO' that is instance of @MonadIO@.
--
-- NOTE: IT IS IMPORTANT THAT @UnsafeLIO@ REMAINS HIDDEN AND NO
-- EXPORTED WRAPPER BE MADE AN INSTATNCE OF @MonadLIO@.
newtype UnsafeLIO l p s a = UnsafeLIO { unUnsafeLIO :: LIO l p s a }
  deriving (Functor, Applicative, Monad)

-- | UNSAFE: Instance of @MonadIO@.
instance LabelState l p s => MonadIO (UnsafeLIO l p s) where
  liftIO = UnsafeLIO . rtioTCB

-- | An LIO action with MongoDB access.
newtype LIOAction l p s a =
    LIOAction { unLIOAction :: M.Action (UnsafeLIO l p s) a }
  deriving (Functor, Applicative, Monad)

newtype Action l p s a = Action (ReaderT (Collection l) (LIOAction l p s) a)
  deriving (Functor, Applicative, Monad)

-- | Run action against database on server at other end of pipe. Use
-- access mode for any reads and writes. Return 'Left' on connection
-- failure or read/write failure.
-- The current label is raised to the the join of the database label
-- and current label.
--
-- TODO: make sure that Failure does not leak any information.
access :: LabelState l p s
       => Pipe
       -> M.AccessMode
       -> Collection l
       -> Action l p s a
       -> LIO l p s (Either M.Failure a)
access = accessP noPrivs

-- | Same as 'access', but uses privileges when raising the current
-- label.
accessP :: LabelState l p s
        => p 
        -> Pipe
        -> M.AccessMode
        -> Collection l
        -> Action l p s a
        -> LIO l p s (Either M.Failure a)
accessP p' pipe mode col (Action act) = withCombinedPrivs p' $ \p -> do 
  taintP p (dbLabel db)
  let lioAct = runReaderT act col
  unUnsafeLIO $ M.access pipe mode (dbIntern db) (unLIOAction lioAct)
	where db = colDatabase col

--
-- Serializing 'Value's
--

-- | Convert a 'Document' to a Bson @Document@. It is an error to call
-- this function with malformed 'Document's (i.e., those for which
-- a policy has not been applied.
toBsonDoc :: (Serialize l, Label l) => Document l -> M.Document
toBsonDoc = map (\(k := v) -> (k M.:= toBsonValue v)) . exceptInternal

-- | Convert a Bson @Document@ to a 'Document'. This implementation is
-- relaxed and omits any fields that were not converted. Use the
-- 'fromBsonDocStrict' for a strict conversion. 
fromBsonDoc :: (Serialize l, Label l) => M.Document -> Document l
fromBsonDoc d = 
  let cs' = map (\(k M.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
  in exceptInternal $ cs

-- | Same as 'fromBsonDoc', but fails (returns @Nothing@) if any of
-- the field  values failed to be serialized.
fromBsonDocStrict :: (Serialize l, Label l) => M.Document -> Maybe (Document l)
fromBsonDocStrict d = 
  let cs' = map (\(k M.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
      ok  = all (isJust .snd) cs'
  in if ok then Just . exceptInternal $ cs else Nothing


-- | Remove any fields from the document that have
-- 'hailsInternalKeyPrefix' as a prefix
exceptInternal :: Label l => Document l -> Document l
exceptInternal [] = []
exceptInternal (f@(k := _):fs) =
  let rest = exceptInternal fs
  in if hailsInternalKeyPrefix `isPrefixOf` k
       then rest
       else f:rest
                                


-- | This prefix is reserved for HAILS keys. It should not be used by
-- arbitrary code.
hailsInternalKeyPrefix :: M.Label
hailsInternalKeyPrefix = u "__hails_internal_"

-- | Serializing a 'Labeled' to a BSON @Document@ with key 
-- @lBsonLabeledValKey@.
lBsonLabeledValKey :: M.Label
lBsonLabeledValKey = hailsInternalKeyPrefix `append` u "Labeled"

-- | Serializing a 'PolicyLabeled' to a BSON @Document@ with key 
-- @lBsonPolicyLabeledValKey@.
lBsonPolicyLabeledValKey :: M.Label
lBsonPolicyLabeledValKey = hailsInternalKeyPrefix `append` u "PolicyLabeled"

-- | When serializing a 'Labeled' we serialize it to a document
-- containing the label and value, the key for the label is
-- @lBsonLabelKey@.
lBsonLabelKey :: M.Label
lBsonLabelKey = u "label"

-- | When serializing a 'Labeled' (or 'PolicyLabeled') we serialize
-- it to a document containing the value, the key for the value
-- is @lBsonValueKey@.
lBsonValueKey :: M.Label
lBsonValueKey = u "value"

-- | Convert 'Value' to Bson @Value@
toBsonValue :: (Serialize l, Label l) => Value l -> M.Value
toBsonValue mV = 
  case mV of 
    (BsonVal v)            -> v
    (LabeledVal lv) -> M.val [ lBsonLabeledValKey M.=:
              [ lBsonLabelKey M.=: Binary (encode (labelOf lv))
              , lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PL lv)) -> M.val [ lBsonPolicyLabeledValKey M.=:
              [ lBsonValueKey M.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PU _)) -> error "bsonValue2lbsonValue: Invalid use."

-- | Convert Bson @Value@ to 'Value'
fromBsonValue :: (Serialize l, Label l) => M.Value -> Maybe (Value l)
fromBsonValue mV = do
  case mV of
    x@(M.Doc d) ->
      let haveL = isJust $ M.look lBsonLabeledValKey d
          havePL = isJust $ M.look lBsonPolicyLabeledValKey d
      in if haveL || havePL
           then getLabeled d `orMaybe` getPolicyLabeled d
           else Just (BsonVal x)
    x         -> Just (BsonVal x)
  where getLabeled :: (Serialize l, Label l) => M.Document -> Maybe (Value l)
        getLabeled d = do
          (M.Doc lv) <- M.look lBsonLabeledValKey d
          (Binary b) <- M.lookup lBsonLabelKey lv
          l <- either (const Nothing) return (decode b)
          v <- M.look lBsonValueKey lv
          return . LabeledVal $ labelTCB l v
        --
        getPolicyLabeled :: (Serialize l, Label l) => M.Document -> Maybe (Value l)
        getPolicyLabeled d = do
          (M.Doc lv) <- M.look lBsonPolicyLabeledValKey d
          v <- M.look lBsonValueKey lv
          return . PolicyLabeledVal . PU $ v
        --
        orMaybe :: Maybe a -> Maybe a -> Maybe a
        orMaybe x y = if isJust x then x else y
