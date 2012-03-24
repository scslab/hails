{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{- | This module exports an interface for LBSON (Labeled BSON) object.
   An LBSON object is either a BSON object (see 'Data.Bson') with the
   added support for labeled 'Value's. More specifically, a LBSON
   document is a list of 'Field's (which are 'Key'-'Value' pairs),
   where the 'Value' of a 'Field' can either be a standard
   'Data.Bson.Value' type or a 'Labeled' 'Value' type.
-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

--
#define DEBUG 0
--

module Hails.Data.LBson.TCB ( -- * UTF-8 String
                              module Data.UString
                              -- * Document
                            , Document, LabeledDocument
                            , look, lookup, valueAt, at, include, exclude, merge
                              -- * Field
                            , Field(..), (=:), (=?)
                            , Key
                            , hailsInternalKeyPrefix 
                            , isUnsafeKey
                              -- * Value
                            , Value(..), Val(..), cast, typed
                              -- * Policy labeled values
                            , PolicyLabeled(..), pu, pl
                              -- * Special Bson value types
                            , Binary(..)
                            , Function(..)
                            , UUID(..)
                            , MD5(..)
                            , UserDefined(..)
                            , Regex(..)
                            , Javascript(..)
                            , Symbol(..)
                            , MongoStamp(..)
                            , MinMaxKey(..)
                              -- ** ObjectId
                            , ObjectId(..)
                            , timestamp
                            , genObjectId
                            -- * Serializing Value, converting to Bson documents
                            , BsonValue, safeToBsonValue, safeFromBsonValue
                            , BsonDocument, safeToBsonDoc, safeFromBsonDoc
                            , encodeDoc, decodeDoc
                            -- Unsafe converters
                            , toBsonDoc
                            , fromBsonDoc, fromBsonDocStrict
                            , sanitizeBsonValue
                            ) where


import Prelude hiding (lookup,)
import Data.UString (UString, u, unpack)
import qualified Data.Bson as Bson
import Data.Bson ( Binary(..)
                 , Function(..)
                 , UUID(..)
                 , MD5(..)
                 , UserDefined(..)
                 , Regex(..)
                 , Javascript(..)
                 , Symbol(..)
                 , MongoStamp(..)
                 , MinMaxKey(..)
                 , ObjectId(..)
                 , timestamp)
import Data.Bson.Binary (putDocument, getDocument)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as L

import Data.Maybe
import Data.List (find, findIndex)
import Data.Typeable hiding (cast)
import Data.CompactString.UTF8 (append, isPrefixOf)
import Data.Serialize (Serialize, encode, decode)

import Control.Monad
import Control.Monad.Identity (runIdentity)

import LIO
import LIO.TCB (labelTCB, unlabelTCB, rtioTCB)
#if DEBUG
import LIO.TCB (showTCB)
#endif

--
-- Document related
--


-- | A 'Key', or attribute is a BSON label.
type Key = Bson.Label

-- | A LBSON document is a list of 'Field's
type Document l = [Field l]

-- | A labeled 'Document'
type LabeledDocument l = Labeled l (Document l)

-- | Value of field in document, or fail (Nothing) if field not found
look :: (Monad m, Label l) => Key -> Document l -> m (Value l)
look k doc = maybe notFound (return . value) (find ((k ==) . key) doc)
  where notFound = fail $ "expected " ++ show k

-- | Lookup value of field in document and cast to expected
-- type. Fail (Nothing) if field not found or value not of expected
-- type.
lookup :: (Val l v, Monad m, Label l) => Key -> Document l -> m v
lookup k doc = cast =<< look k doc


-- | Value of field in document. Error if missing.
valueAt :: Label l => Key -> [Field l] -> Value l
valueAt k = runIdentity . look k

-- | Typed value of field in document. Error if missing or wrong type.
at :: forall v l. (Val l v, Label l) => Key -> Document l -> v
at k doc = fromMaybe err (lookup k doc)
  where err = error $ "expected (" ++ show k ++ " :: "
                ++ show (typeOf (undefined :: v)) ++ ") in " ++ show doc

-- | Only include fields of document in key list
include :: Label l => [Key] -> Document l -> Document l
include keys doc = mapMaybe (\k -> find ((k ==) . key) doc) keys

-- | Exclude fields from document in key list
exclude :: Label l => [Key] -> Document l -> Document l
exclude keys = filter (\(k := _) -> notElem k keys)

-- | Merge documents with preference given to first one when both
-- have the same key. I.e. for every (k := v) in first argument,
-- if k exists in second argument then replace its value with v,
-- otherwise add (k := v) to second argument.
merge :: Label l => Document l -> Document l -> Document l
merge es doc' = foldl f doc' es where
	f doc (k := v) = case findIndex ((k ==) . key) doc of
		Nothing -> doc ++ [k := v]
		Just i -> let (x, _ : y) = splitAt i doc in x ++ [k := v] ++ y

--
-- Field related
--

infix 0 :=, =:, =?

-- | A @Field@ is a 'Key'-'Value' pair.
data Field l = (:=) { key :: !Key
                    , value :: Value l }
                    deriving (Eq, Typeable)

instance Label l => Show (Field l) where
  showsPrec d (k := v) = showParen (d > 0) $
    showString (' ' : unpack k) . showString ": " . showsPrec 1 v



-- | Field with given label and typed value
(=:) :: (Val l v, Label l) => Key -> v -> Field l
k =: v = k := val v

-- | If @Just@ value then return one field document, otherwise
-- return empty document
(=?) :: (Val l a, Label l) => Key -> Maybe a -> Document l
k =? ma = maybeToList (fmap (k =:) ma)

--
-- Value related
--

-- | A @Value@ is either a standard BSON value, a labeled value, or
-- a policy-labeled value.
data Value l = BsonVal Bson.Value
             -- ^ Unlabeled BSON value
             | LabeledVal (Labeled l Bson.Value)
             -- ^ Labeled (LBSON) value
             | PolicyLabeledVal (PolicyLabeled l Bson.Value)
             -- ^ Policy labeled (LBSON) value
             deriving (Typeable)

-- | Instance for @Show@, only showing unlabeled BSON values.
instance Label l => Show (Value l) where
  show (BsonVal v) = show v
#if DEBUG
  show (LabeledVal lv) = showTCB lv
  show (PolicyLabeledVal lv) = show lv
#else
  show _ = "{- HIDING DATA -} "
#endif

-- | Instance for @Eq@, only comparing unlabeled BSON values.
instance Label l => Eq (Value l) where
  (==) (BsonVal v1) (BsonVal v2) = v1 == v2
  (==) _ _ = False


-- | Haskell types of this class correspond to LBSON value types.
class (Typeable a, Show a, Eq a, Label l) => Val l a where
  val   :: a -> Value l
  cast' :: Value l -> Maybe a

-- | Every type that is an instance of BSON Val is an instance of
-- LBSON Val. This requires the use of @OverlappingInstances@
-- extension.
instance (Bson.Val a, Label l) => Val l a where
  val   = BsonVal . Bson.val
  cast' (BsonVal v) = Bson.cast' v
  cast' _           = Nothing
              
-- | Every 'Value' is a 'Val'.
instance (Label l) => Val l (Value l) where
  val   = id
  cast' = Just

-- | Convert between a labeled value and a labeled BSON value.
instance (Bson.Val a, Label l) => Val l (Labeled l a) where
  val lv = let l = labelOf lv
               v = unlabelTCB lv
           in LabeledVal $ labelTCB l (Bson.val v)
  cast' (LabeledVal lv) = let l = labelOf lv
                              v = unlabelTCB lv
                          in Bson.cast' v >>= return . labelTCB l
  cast' _ = Nothing

-- | Convert between a policy-labeled value and a labeled BSON value.
instance (Bson.Val a, Label l) => Val l (PolicyLabeled l a) where
  val (PU x) = PolicyLabeledVal . PU . Bson.val $ x
  val (PL lv) = let l = labelOf lv
                    v = unlabelTCB lv
                in PolicyLabeledVal . PL $ labelTCB l (Bson.val v)
  cast' (PolicyLabeledVal (PU v)) = Bson.cast' v >>= return . PU
  cast' (PolicyLabeledVal (PL lv)) = let l = labelOf lv
                                         v = unlabelTCB lv
                                     in Bson.cast' v >>=
                                        return . PL . labelTCB l
  cast' _ = Nothing


-- | Convert Value to expected type, or fail (Nothing) if not of that type
cast :: forall m l a. (Label l, Val l a, Monad m) => Value l -> m a
cast v = maybe notType return (cast' v)
  where notType = fail $ "expected " ++ show (typeOf (undefined :: a))
                                     ++ ": " ++ show v


-- | Convert Value to expected type. Error if not that type.
typed :: (Val l a, Label l) => Value l -> a
typed = runIdentity . cast

--
-- Misc.
--


-- | Necessary instance that just fails.
instance (Show a, Label l) => Show (Labeled l a) where
#if DEBUG
  show = showTCB 
#else
  show = error "Instance of show for Labeled not supported"
#endif

-- | Necessary instance that just fails.
instance Label l => Eq (Labeled l a) where
  (==)   = error "Instance of Eq for Labeled not supported"

-- | Generate fresh 'ObjectId'.
genObjectId :: LabelState l p s => LIO l p s ObjectId
genObjectId = rtioTCB $ Bson.genObjectId


--
-- Policy labeled values
--

-- | Simple sum type used to denote a policy-labeled type. A
-- @PolicyLabeled@ type can be either labeled (policy applied),
-- or unabled (policy not yet applied).
data PolicyLabeled l a = PU a             -- ^ Policy was not applied 
                       | PL (Labeled l a) -- ^ Policy applied
                       deriving (Typeable)

-- | Wrap an unlabeled value by 'PolicyLabeled'.
pu :: (Label l, Bson.Val a) => a -> PolicyLabeled l a
pu = PU

-- | Wrap an already-labeled value by 'PolicyLabeled'.
pl :: (Label l, Bson.Val a) => Labeled l a -> PolicyLabeled l a
pl = PL

-- | Necessary instance that just fails.
instance (Show a, Label l) => Show (PolicyLabeled l a) where
#if DEBUG
  show (PU x) = show x 
  show (PL x) = showTCB x 
#else
  show = error "Instance of show for PolicyLabeled not supported"
#endif

-- | Necessary instance that just fails.
instance Label l => Eq (PolicyLabeled l a) where
  (==) = error "Instance of show for PolicyLabeled not supported"

--
-- Serializing 'Value's
--

-- | Export 'Bson.Value'
type BsonValue = Bson.Value

-- | Safely convert from a 'Value' to a 'BsonValue'.
safeToBsonValue :: (Label l) => Value l -> Maybe BsonValue
safeToBsonValue (BsonVal v) = Just v
safeToBsonValue _  = Nothing

-- | Safely convert from a 'BsonValue' to a 'Value'.
safeFromBsonValue :: (Serialize l, Label l) => BsonValue -> Maybe (Value l)
safeFromBsonValue v' = case fromBsonValue v' of
  mv@(Just (BsonVal _)) -> mv
  _                     -> Nothing

-- | Export 'Bson.Document'
type BsonDocument = Bson.Document

-- | Safe version of 'toBsonDoc'.
safeToBsonDoc :: (Serialize l, Label l) => Document l -> Maybe BsonDocument
safeToBsonDoc = mapM (\(k := v) -> do v' <- safeToBsonValue v
                                      return (k Bson.:= v')) . exceptInternal

-- | Safe version of 'fromBsonDoc'.
safeFromBsonDoc :: (Serialize l, Label l) => BsonDocument -> Maybe (Document l)
safeFromBsonDoc d = do
  d' <- forM d $ \(k Bson.:= v) -> do v' <- safeFromBsonValue v
                                      return (k := v')
  return $ exceptInternal d'

-- | Convert a 'Document' to a Bson @Document@. It is an error to call
-- this function with malformed 'Document's (i.e., those for which
-- a policy has not been applied.
toBsonDoc :: (Serialize l, Label l) => Document l -> Bson.Document
toBsonDoc = map (\(k := v) -> (k Bson.:= toBsonValue v)) . exceptInternal

-- | Convert a Bson @Document@ to a 'Document'. This implementation is
-- relaxed and omits any fields that were not converted. Use the
-- 'fromBsonDocStrict' for a strict conversion. 
fromBsonDoc :: (Serialize l, Label l) => Bson.Document -> Document l
fromBsonDoc d = 
  let cs' = map (\(k Bson.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
  in exceptInternal cs

-- | Same as 'fromBsonDoc', but fails (returns @Nothing@) if any of
-- the field  values failed to be serialized.
fromBsonDocStrict :: (Serialize l, Label l)
                  => Bson.Document -> Maybe (Document l)
fromBsonDocStrict d = 
  let cs' = map (\(k Bson.:= v) -> (k, fromBsonValue v)) d
      cs  = map (\(k, Just v) -> k := v) $ filter (isJust . snd) cs'
      ok  = all (isJust .snd) cs'
  in if ok then Just . exceptInternal $ cs else Nothing

-- | Check if a key is unsafe.
isUnsafeKey :: Key -> Bool
isUnsafeKey k = or
  [ hailsInternalKeyPrefix `isPrefixOf` k 
  , (u "$") `isPrefixOf` k
  ]

-- | If value is a document, remove any fields that have
-- 'hailsInternalKeyPrefix' as a prefix, otherwise return the value
-- unchanged. This is equivilant to 'exceptInternal' except it
-- operates on BSON values as opposed to Hails Documents.
sanitizeBsonValue :: Bson.Value -> Bson.Value
sanitizeBsonValue (Bson.Doc doc) = Bson.Doc $ doExcludes doc
  where doExcludes [] = []
        doExcludes (f@(k Bson.:= _):fs) =
          let rest = doExcludes fs
          in if isUnsafeKey k
               then rest
               else f:rest
sanitizeBsonValue v = v

-- | Remove any fields from the document that have
-- 'hailsInternalKeyPrefix' as a prefix
exceptInternal :: Label l => Document l -> Document l
exceptInternal [] = []
exceptInternal (f@(k := _):fs) =
  let rest = exceptInternal fs
  in if isUnsafeKey k
       then rest
       else f:rest

-- | This prefix is reserved for HAILS keys. It should not be used by
-- arbitrary code.
hailsInternalKeyPrefix :: Key
hailsInternalKeyPrefix = u "__hails_internal_"

-- | Serializing a 'Labeled' to a BSON @Document@ with key 
-- @lBsonLabeledValKey@.
lBsonLabeledValKey :: Key
lBsonLabeledValKey = hailsInternalKeyPrefix `append` u "Labeled"

-- | Serializing a 'PolicyLabeled' to a BSON @Document@ with key 
-- @lBsonPolicyLabeledValKey@.
lBsonPolicyLabeledValKey :: Key
lBsonPolicyLabeledValKey = hailsInternalKeyPrefix `append` u "PolicyLabeled"

-- | When serializing a 'Labeled' we serialize it to a document
-- containing the label and value, the key for the label is
-- @lBsonLabelKey@.
lBsonLabelKey :: Key
lBsonLabelKey = u "label"

-- | When serializing a 'Labeled' (or 'PolicyLabeled') we serialize
-- it to a document containing the value, the key for the value
-- is @lBsonValueKey@.
lBsonValueKey :: Key
lBsonValueKey = u "value"

-- | Convert 'Value' to Bson @Value@
toBsonValue :: (Serialize l, Label l) => Value l -> Bson.Value
toBsonValue mV = 
  case mV of 
    (BsonVal v)            -> v
    (LabeledVal lv) -> Bson.val [ lBsonLabeledValKey Bson.=:
              [ lBsonLabelKey Bson.=: Binary (encode (labelOf lv))
              , lBsonValueKey Bson.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PL lv)) -> Bson.val [ lBsonPolicyLabeledValKey Bson.=:
              [ lBsonValueKey Bson.:= unlabelTCB lv ] ]
    (PolicyLabeledVal (PU _)) -> error "toBsonValue: Invalid use (PU _)."


-- | Convert Bson @Value@ to 'Value'
fromBsonValue :: (Serialize l, Label l) => Bson.Value -> Maybe (Value l)
fromBsonValue mV =
  case mV of
    x@(Bson.Doc d) ->
      let haveL = isJust $ Bson.look lBsonLabeledValKey d
          havePL = isJust $ Bson.look lBsonPolicyLabeledValKey d
      in if haveL || havePL
           then getLabeled d `orMaybe` getPolicyLabeled d
           else Just (BsonVal x)
    x         -> Just (BsonVal x)
  where getLabeled :: (Serialize l, Label l) => Bson.Document -> Maybe (Value l)
        getLabeled d = do
          (Bson.Doc lv) <- Bson.look lBsonLabeledValKey d
          (Binary b) <- Bson.lookup lBsonLabelKey lv
          l <- either (const Nothing) return (decode b)
          v <- Bson.look lBsonValueKey lv
          return . LabeledVal $ labelTCB l v
        --
        getPolicyLabeled :: (Serialize l, Label l)
                         => Bson.Document -> Maybe (Value l)
        getPolicyLabeled d = do
          (Bson.Doc lv) <- Bson.look lBsonPolicyLabeledValKey d
          v <- Bson.look lBsonValueKey lv
          return . PolicyLabeledVal . PU $ v
        --
        orMaybe :: Maybe a -> Maybe a -> Maybe a
        orMaybe x y = if isJust x then x else y


--
-- Encoding/decoding Bson documents
--

-- | Class used to encode/decode 'BsonDocument's, and 'Document's that
-- do not have 'PolicyLabeled' or 'Labeled' values.
class BsonDocSerialize doc where
  encodeDoc :: doc -> L.ByteString
  -- ^ Encodea document
  decodeDoc :: L.ByteString -> doc
  -- ^ Decode a document

instance BsonDocSerialize BsonDocument where
  encodeDoc doc' = let (Bson.Doc doc) = sanitizeBsonValue . Bson.Doc $ doc'
                   in runPut $ putDocument doc
  decodeDoc bs = let (Bson.Doc doc) = sanitizeBsonValue
                                    . Bson.Doc $ runGet getDocument bs
                 in doc

instance (Serialize l, Label l) => BsonDocSerialize  (Document l) where
  encodeDoc = encodeDoc . fromJust . safeToBsonDoc
  decodeDoc = fromJust . safeFromBsonDoc . decodeDoc 
