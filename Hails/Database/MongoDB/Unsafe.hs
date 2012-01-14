{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- TODO: remove:
{-# LANGUAGE OverloadedStrings #-}
--
module Hails.Database.MongoDB.Unsafe where

import Prelude hiding (lookup)
import LIO
import LIO.MonadCatch
import Hails.Data.LBson
import Data.Typeable
import qualified Data.List as List
import qualified Database.MongoDB as M
import qualified Control.Exception as E

import Control.Monad (unless, forM)


-- TODO: remove
import LIO.DCLabel
import LIO.TCB (ioTCB)
import Data.Monoid (mempty)
main :: IO ()
main = ignore $ evalDC $ do
  let n = "w00t" :: String
      p = "p455w0rd" :: String
      c = Collection { colLabel = lbot :: DCLabel
                     , colClear = ltop
                     , colIntern = "auth"
                     , colPolicy = RawPolicy {
                         rawDocPolicy = \doc -> newDC (<>) (<>)
                       , rawFieldPolicies = [
                            ( "password", \doc -> let name = at "name" doc :: String
                                                  in newDC (name) (name))
                          ]
                       }
                     }

      x = [ "name" =: n
          , "password" =: (pl p :: PolicyLabeled DCLabel String)
          ] :: Document DCLabel
  lx <- applyRawPolicyP mempty c x
  ioTCB $ print lx
  ioTCB $ print x
  return ()

ignore io = io >> return ()
--



--
-- Collections
--

-- | A collection is a MongoDB collection associated with
-- label and clearance and labeling policy for collection documents.
-- The label of the collection is used to enforce who can write
-- to it (i.e., current label must flow to the label of the
-- collection). Moreover the label imposes the lowest label of
-- the data that can be written to the collection (important
-- when integrity is a concern). The clearance of the collection
-- is used to limit the sensitivity of the data written to the
-- collection (i.e., label of data must flow to the clearance of
-- the collection). The label on the collection label and clearance
-- is the current label, i.e., they are public.
data Collection l = Collection { colLabel  :: l
                               -- ^ Collection label
                               , colClear  :: l
                               -- ^ Collection clearance
                               , colIntern :: M.Collection
                               -- ^ Actual MongoDB collection
                               , colPolicy :: RawPolicy l
                               -- ^ Collection labeling policy
                               }


--
-- Databases
--


-- | A database has a label, which is used to enforce who can write to
-- the database, and an internal identifier corresponding to the underlying
-- MongoDB database.
data DB l = DB { dbLabel  :: l          -- ^ Label of database
               , dbIntern :: M.Database -- ^ Actual MongoDB database
               }
                                 
--
-- Policies 
--


-- | A @RawPolicy@ encodes a document/row policy, and all
-- field/column policies. It is required that all fields of type
-- 'PolicyLabled' have a field/column policy -- if using only this
-- low-level interface a runtime-error will occur if this is not
-- satisfied.
data RawPolicy l = RawPolicy {
      rawDocPolicy     :: Document l -> l
    -- ^ A row (document) policy is a function from a 'Document' to a 'Label'.
    , rawFieldPolicies :: [(Key, Document l -> l)]
    -- ^ A column (field) policy is a function from a 'Document' to a
    -- 'Label', for each field of type 'PolicyLabeled'.
  }


{-
applyRawPolicyP :: LabelState l p s =>
  p -> Collection l a -> Document l -> LIO l p s (Labeld l (Document l))
applyRawPolicyP p' = withCombinedPrivs p' $ \p -> do
  
-}



-- | Apply a raw field/column policy to the field corresponding to the
-- key. If the policy has not been specified for this key, the function
-- throws an exception. Similarly, if the policy has already been
-- applied for this key and the label existing label does not match the
-- newly policy-generated label, an exception is thrown.
applyRawFieldPolicyP :: (LabelState l p s)
                     => p 
                     -> Collection l
                     -> Document l
                     -> Key
                     -> LIO l p s (Field l)
applyRawFieldPolicyP p' col doc k = withCombinedPrivs p' $ \p -> do
  let policies = rawFieldPolicies . colPolicy $ col
  f <- maybe (throwIO NoFieldPolicy) return $ List.lookup k policies 
  plv <- maybe (throwIO InvalidPolicy) return $ lookPL
  lv <- case plv of
         (PU v)  -> labelP p (f doc) v
         (PL lv) -> do unless (labelOf lv /= (f doc)) $ throwIO PolicyViolation
                       return lv
  return (k := (PolicyLabeledVal . PL $ lv))
    where lookPL = case look k doc of
            (Just (PolicyLabeledVal x)) -> Just x
            _                           -> Nothing

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled'.
applyRawFieldPoliciesP :: (LabelState l p s)
                       => p 
                       -> Collection l
                       -> Document l
                       -> LIO l p s (Document l)
applyRawFieldPoliciesP p' col doc = withCombinedPrivs p' $ \p -> do
  forM doc $ \field@(k := v) ->
    case v of
      (PolicyLabeledVal _) -> applyRawFieldPolicyP p col doc k
      _ -> return field

-- | Apply a raw field/column policy to all the fields of type
-- 'PolicyLabeled', and then apply the raw document/row policy
-- It must be that every labeled value in the document have a label
-- that is between the label and clearance of the collection.
applyRawPolicyP :: (LabelState l p s)
                => p 
                -> Collection l
                -> Document l
                -> LIO l p s (Labeled l (Document l))
applyRawPolicyP p' col doc = withCombinedPrivs p' $ \p -> do
  let lbl = colLabel col
      clr = colClear col
      f   = rawDocPolicy . colPolicy $ col
  doc' <- withClearance clr $ do taintP p lbl
                                 applyRawFieldPoliciesP p' col doc
  guardLabeledVals doc' p lbl clr
  label (f doc') doc'
    where guardLabeledVals [] _ _  _ = return ()
          guardLabeledVals ((_ := v):ds) p l c = do
            case v of
              (LabeledVal lv) -> do let newl = labelOf lv
                                    unless (leqp p newl c) $ throwIO LerrClearance
                                    unless (leqp p l newl) $ throwIO LerrLow
              _ -> return ()
            guardLabeledVals ds p l c

--
-- Exceptions
--

-- | Field/column policies are required for every 'PolicyLabled' value
-- in a document.
data PolicyError = NoFieldPolicy   -- ^ Policy for field no specified
                 | InvalidPolicy   -- ^ Policy application invalid
                 | PolicyViolation -- ^ Policy has been violated
  deriving (Typeable)

instance Show PolicyError where
  show NoFieldPolicy = "NoFieldPolicy: Field policy not found"
  show InvalidPolicy = "InvalidPolicy: Invalid policy application"
  show PolicyViolation = "PolicyViolation:  policy violated"

instance E.Exception PolicyError
