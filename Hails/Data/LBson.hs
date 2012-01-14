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

module Hails.Data.LBson ( -- * UTF-8 String
                          module Data.UString
                          -- * Document
                        , Document
                        , look, lookup, valueAt, at, include, exclude, merge
                          -- * Field
                        , Field(..), (=:), (=?)
                        , Key
                          -- * Value
                        , Value(..), Val(..), cast, typed
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
                 , timestamp
                 , genObjectId)

import LIO
import LIO.TCB (labelTCB, unlabelTCB)

import Data.Maybe (mapMaybe, maybeToList)
import Data.List (find, findIndex)
import Data.Typeable hiding (cast)

import Control.Monad.Identity (runIdentity)

--
-- Document related
--


-- | A 'Key', or attribute is a BSON label.
type Key = Bson.Label

-- | A LBSON document is a list of 'Field's
type Document l = [Field l]

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
at k doc = maybe err id (lookup k doc)
  where err = error $ "expected (" ++ show k ++ " :: "
                ++ show (typeOf (undefined :: v)) ++ ") in " ++ show doc

-- | Only include fields of document in key list
include :: Label l => [Key] -> Document l -> Document l
include keys doc = mapMaybe (\k -> find ((k ==) . key) doc) keys

-- | Exclude fields from document in key list
exclude :: Label l => [Key] -> Document l -> Document l
exclude keys doc = filter (\(k := _) -> notElem k keys) doc

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

-- | A @Value@ is either a standard BSON value, or a labeled value.
data Value l = BSonVal Bson.Value               -- ^ Unlabeled BSON value
             | LabeledVal (Labeled l (Value l)) -- ^ Labeled (BSON) value
             deriving (Typeable)

-- | Instance for @Show@, only showing unlabeled BSON values.
instance Label l => Show (Value l) where
  show (BSonVal v) = show v
  show (LabeledVal lv) = show (labelOf lv) ++ "{- HIDING DATA -} "

-- | Instance for @Eq@, only comparing unlabeled BSON values.
instance Label l => Eq (Value l) where
  (==) (BSonVal v1) (BSonVal v2) = v1 == v2
  (==) _ _ = False


-- | Haskell types of this class correspond to LBSON value types.
class (Typeable a, Show a, Eq a, Label l) => Val l a where
  val   :: a -> Value l
  cast' :: Value l -> Maybe a

-- | Every type that is an instance of BSON Val is an instance of
-- LBSON Val.
instance (Bson.Val a, Label l) => Val l a where
  val   = BSonVal . Bson.val
  cast' (BSonVal v) = Bson.cast' v
  cast' _           = Nothing
              

-- | Convert between a labeled value and a labeled BSON value.
instance (Val l a, Label l) => Val l (Labeled l a) where
  val lv = let l = labelOf lv
               v = unlabelTCB lv
           in LabeledVal $ labelTCB l (val v)
  cast' (BSonVal _ ) = Nothing
  cast' (LabeledVal lv) = let l = labelOf lv
                              v = unlabelTCB lv
                          in cast' v >>= return . labelTCB l


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
instance Label l => Show (Labeled l a) where
  show l = error "Instance of show for Labeled not supported"
-- | Necessary instance that just fails.
instance Label l => Eq (Labeled l a) where
  (==)   = error "Instance of Eq for Labeled not supported"
