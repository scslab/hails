{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | This module exports a class 'Groups' that policy modules
must define an instance of to define groups, or mappings
between a group 'Principal'and the principals in the group.

An app may then relabel a labeled value by using 'labelRewrite'.


-}
module Hails.PolicyModule.Groups ( Groups(..)
                                 , labelRewrite ) where

import           Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import           Control.Monad

import           LIO
import           LIO.DCLabel

import           Hails.Database
import           Hails.Database.TCB (dbActionPriv, getActionStateTCB)
import           Hails.PolicyModule


class PolicyModule pm => Groups pm where
  -- | Typically, the action should expand a principal such as @#group@ to
  -- list of group members @[alice, bob]@.
  groups :: pm                   -- ^ Unused type-enforcing param
         -> DCPriv               -- ^ Policy module privs
         -> Principal            -- ^ Group
         -> DBAction [Principal] -- ^ (Policy module, group members)
  -- | Endorse the implementation of this instance. Note that this is
  -- reduced to WHNF to catch invalid instances that use 'undefined'.
  --
  -- Example implementation:
  --
  -- > groupsInstanceEndorse _ = MyPolicyModuleTCB {- Leave other values undefined -}
  groupsInstanceEndorse :: pm

-- | Given the policy module (which is used to invoke the right
-- 'groups' function) and labeled value, relabel the value according
-- to the 'Groups' of the policy module. Note that the first argument
-- may be bottom since it is solely used for typing purposes.
labelRewrite :: forall unused_pm a. Groups unused_pm
             => unused_pm
             -- ^ Policy module
             -> DCLabeled a
             -- ^ Label
             -> DBAction (DCLabeled a)
labelRewrite pm lx = do
  -- Make sure that 'groupsInstanceEndorse' is not bottom
  _ <- liftLIO $ evaluate (groupsInstanceEndorse :: unused_pm)
  -- Get underlying privileges if they corresponds to the named (by
  -- the first argument) policy module
  pmPriv <- getPMPriv
  -- Build map from principals to list of princpals
  pmap <- forM principals $ \p -> groups pm pmPriv p >>= \ps -> return (p, ps)
  -- Apply map to all principals in the label
  let lnew = dcLabel (mk pmap s) (mk pmap i)
  -- Relabel labeled value
  relabelLabeledP pmPriv lnew lx
    where getPMPriv = do
            pmPriv <- dbActionPriv `liftM` getActionStateTCB
            -- Make sure that the underlying policy module
            -- and one named in the first parameter are the same
            case Map.lookup (policyModuleTypeName pm) availablePolicyModules of
              Nothing -> return noPriv
              Just (p,_) -> return $ if toComponent p == privDesc pmPriv
                                       then pmPriv
                                       else noPriv
          -- Modify label by expanding principals according to the map
          mk pmap lc =
            let f = map (concatMap (\x -> fromJust $ List.lookup x pmap))
            in if lc == dcFalse
                 then lc
                 else fromList . f . toList $ lc
          -- Label components
          s = dcSecrecy $ labelOf lx
          i = dcIntegrity $ labelOf lx
          -- All unique principals in the labe
          principals = List.nub $ getPrincipals s ++ getPrincipals i
          -- Get principals form component
          getPrincipals lc = if lc == dcFalse
                              then []
                              else List.nub . concat . toList $ lc
