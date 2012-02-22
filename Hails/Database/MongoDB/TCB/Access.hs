{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

module Hails.Database.MongoDB.TCB.Access ( -- * Policy application
                                           applyRawPolicyP
                                         , applyRawPolicyTCB
                                           -- * Running actions against DB
                                         , access, accessP
                                         ) where

import LIO
import LIO.TCB ( getTCB
               , putTCB
               , setLabelTCB
               , lowerClrTCB
               )
import LIO.MonadCatch
import Hails.Data.LBson.TCB
import Hails.Database.MongoDB.TCB.Types

import qualified Data.List as List
import Database.MongoDB.Connection
import qualified Database.MongoDB as M
import Control.Monad.Error hiding (liftIO)
import Control.Monad.State.Strict hiding (liftIO)

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
                     -> CollectionPolicy l
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
                       -> CollectionPolicy l
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
                -> CollectionPolicy l
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
                  => CollectionPolicy l
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

-- | Run action against database on server at other end of pipe. Use
-- access mode for any reads and writes. Return 'Left' on connection
-- failure or read/write failure.
-- The current label is raised to the the join of the database label
-- and current label.
--
-- TODO: Make sure that Failure does not leak sensitive information.
access :: LabelState l p s
       => Pipe
       -> M.AccessMode
       -> Database l
       -> Action l p s a
       -> LIO l p s (Either M.Failure a)
access = accessP noPrivs

-- | Same as 'access', but uses privileges when raising the current
-- label.
accessP :: LabelState l p s
        => p 
        -> Pipe
        -> M.AccessMode
        -> Database l
        -> Action l p s a
        -> LIO l p s (Either M.Failure a)
accessP p' pipe mode db (Action act) = withCombinedPrivs p' $ \p -> do 
  taintP p (dbLabel db)
  let lioAct = evalStateT act db
  unUnsafeLIO $ M.access pipe mode (dbIntern db) (unLIOAction lioAct)


