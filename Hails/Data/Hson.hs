{-# LANGUAGE DeriveDataTypeable,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             OverlappingInstances
             #-}

{- |

This module exports the type for a Hails BSON document, 'HsonDoc' and
related classes for creating such documents.  A Hails document is
similar to "Data.Bson"\'s documents, but differs in two ways. First,
Hails restricts the number of types to a subset of BSON's (see
'BsonVal'). This restriction is primarily due to the fact that many of
the BSON types are redundant and not used (at least within Hails).
Second, Hails allows for documents to contain policy-labeled values.

Policy labeled values ('PolicyLabeled') are permitted only at the
\"top-level\" of a document. (This is primarily done to keep
policy-specification simple and may change in the future.)
Consequently to allow for nested documents and documents containing an
array of values we separate top-level fields ('HsonField'), that may
contain policy labeled values, from potentially-nested fields
('BsonField'). A top-level field 'HsonField' is thus either a
'BsonField' or a 'PolicyLabled' value.

Example:

> module Main (x, y) where
> 
> import Data.Text (Text)
> 
> import LIO.DCLabel
> import LIO.Labeled.TCB (labelTCB)
> import Hails.Data.Hson
> 
> -- | Create document, verbose approach
> x :: HsonDocument
> x = [ "myInt"  =: BsonInt32 42
>     , "nested" =: BsonDoc [ "flag" =: BsonBool True]
>     , "secret" =: (HsonLabeled $ hasPolicy (labelTCB dcPub (BsonString "hi")))
>     ]
> 
> -- | Create same document, clean approach
> y :: HsonDocument
> y = [ "myInt" -: (42 :: Int)
>     , "nested"  -: ([ "flag" -: True] :: BsonDocument)
>     , "secret" -: hasPolicy (labelTCB dcPub (toBsonValue ("hi" :: Text)))
>     ]

Both @x@ and @y@ with @-XOverloadedStrings@:

> [myInt -: 42,nested -: [flag -: True],secret -: HsonLabeled]


-}

module Hails.Data.Hson (
  -- * Documents
    HsonDocument, BsonDocument
  -- * Fields
  , FieldName, HsonField(..), BsonField(..)
  , Field(..), GenField(..)
  -- * Values
  , HsonValue(..), HsonVal(..)
  , BsonValue(..), BsonVal(..)
  , PolicyLabeled, needPolicy, hasPolicy
  , ObjectId(..), Binary(..)
  -- * Converters
  , bsonDocToHsonDoc
  , bsonFieldToHsonField
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int32, Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable

import           LIO.DCLabel

import           Hails.Data.Hson.TCB

infix 0 =:, -:


--
-- Fields
--

instance Show BsonField where
  show (BsonField n v) = T.unpack n ++ " -: " ++ show v

instance Show HsonField where
  show (HsonField n v) = T.unpack n ++ " -: " ++ show v

-- | Class used to define fields.
class (Show v, Show f) => Field v f where
  -- | Given a name and value construct either a 'HsonField' or
  -- 'BsonField'
  (=:) :: FieldName -> v -> f

instance Field BsonValue BsonField where
  (=:) = BsonField
instance Field BsonValue HsonField where
  n =: v = n =: HsonValue v
instance Field HsonValue HsonField where
  (=:) = HsonField

--
-- Values
--


instance Show BsonValue where
  show (BsonFloat v)  = show v
  show (BsonString v) = show v
  show (BsonDoc v)    = show v
  show (BsonArray v)  = show v
  show (BsonBlob v)   = show v
  show (BsonObjId v)  = show v
  show (BsonBool v)   = show v
  show (BsonUTC v)    = show v
  show BsonNull       = "NULL"
  show (BsonInt32 v)  = show v
  show (BsonInt64 v)  = show v

instance Show HsonValue where
  show (HsonValue   h) = show h
  show (HsonLabeled _) = "HsonLabeled"



--
-- Policy labeled values
--

-- | Create a policy labeled value given an unlabeled 'HsonValue'.
needPolicy :: BsonValue-> PolicyLabeled
needPolicy = NeedPolicyTCB

-- | Create a policy labeled value a labeled 'HsonValue'.
hasPolicy :: DCLabeled BsonValue -> PolicyLabeled
hasPolicy = HasPolicyTCB


--
-- Converters
--

-- | Convert 'BsonDocument' to 'HsonDocument'
bsonDocToHsonDoc :: BsonDocument -> HsonDocument
bsonDocToHsonDoc = map bsonFieldToHsonField 

-- | Convert 'BsonField' to 'HsonField'
bsonFieldToHsonField :: BsonField -> HsonField
bsonFieldToHsonField (BsonField n v) = n =: v

--
-- Friendly interface for creating documents
--

-- | Class used to define fields.
class (Show v, Show f) => GenField v f where
  -- | Given a name and Haskell value construct either a 'HsonField'
  -- or a 'BsonField'
  (-:) :: FieldName -> v -> f

instance BsonVal v => GenField v BsonField where
  n -: v = n =: toBsonValue v
instance HsonVal v => GenField v HsonField where
  n -: v = n =: toHsonValue v


--
-- To/From BsonValue
--

-- | Class used to (de)construct 'BsonValue's
class (Typeable a, Show a) => BsonVal a where
  -- | Convert to 'BsonValue'
  toBsonValue   :: a -> BsonValue
  -- | Convert from 'BsonValue'
  fromBsonValue :: BsonValue -> Maybe a


instance BsonVal Double where
  toBsonValue = BsonFloat
  fromBsonValue (BsonFloat x) = Just x
  fromBsonValue (BsonInt32 x) = Just (fromIntegral x)
  fromBsonValue (BsonInt64 x) = Just (fromIntegral x)
  fromBsonValue _ = Nothing

instance BsonVal Float where
  toBsonValue = BsonFloat . realToFrac
  fromBsonValue (BsonFloat x) = Just (realToFrac x)
  fromBsonValue (BsonInt32 x) = Just (fromIntegral x)
  fromBsonValue (BsonInt64 x) = Just (fromIntegral x)
  fromBsonValue _ = Nothing

instance BsonVal Text where
  toBsonValue = BsonString
  fromBsonValue (BsonString x) = Just x
  fromBsonValue _ = Nothing

instance BsonVal String where
  toBsonValue = BsonString . T.pack
  fromBsonValue (BsonString x) = Just $ T.unpack x
  fromBsonValue _ = Nothing

instance BsonVal BsonDocument where
  toBsonValue = BsonDoc
  fromBsonValue (BsonDoc x) = Just x
  fromBsonValue _ = Nothing

instance BsonVal [BsonValue] where
  toBsonValue = BsonArray
  fromBsonValue (BsonArray x) = Just x
  fromBsonValue _ = Nothing

instance (BsonVal a) => BsonVal [a] where
  toBsonValue = BsonArray . map toBsonValue
  fromBsonValue (BsonArray x) = mapM cast x
  fromBsonValue _ = Nothing

instance BsonVal Binary where
  toBsonValue = BsonBlob
  fromBsonValue (BsonBlob x) = Just x
  fromBsonValue _ = Nothing


instance BsonVal ObjectId where
  toBsonValue = BsonObjId
  fromBsonValue (BsonObjId x) = Just x
  fromBsonValue _ = Nothing

instance BsonVal Bool where
  toBsonValue = BsonBool
  fromBsonValue (BsonBool x) = Just x
  fromBsonValue _ = Nothing

instance BsonVal UTCTime where
  toBsonValue = BsonUTC
  fromBsonValue (BsonUTC x) = Just x
  fromBsonValue _ = Nothing

instance BsonVal (Maybe BsonValue) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just v) = v
  fromBsonValue BsonNull = Just Nothing
  fromBsonValue v        = Just (Just v)

instance (BsonVal a) => BsonVal (Maybe a) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just a) = toBsonValue a
  fromBsonValue BsonNull = Just Nothing
  fromBsonValue v        = fmap Just (fromBsonValue v)

instance BsonVal Int32 where
  toBsonValue = BsonInt32
  fromBsonValue (BsonInt32 x) = Just x
  fromBsonValue (BsonInt64 x) = fitInt x
  fromBsonValue (BsonFloat x) = Just (round x)
  fromBsonValue _ = Nothing


instance BsonVal Int64 where
  toBsonValue = BsonInt64
  fromBsonValue (BsonInt64 x) = Just x
  fromBsonValue (BsonInt32 x) = Just (fromIntegral x)
  fromBsonValue (BsonFloat x) = Just (round x)
  fromBsonValue _ = Nothing

instance BsonVal Int where
  toBsonValue n = maybe (BsonInt64 $ fromIntegral n) BsonInt32 (fitInt n)
  fromBsonValue (BsonInt32 x) = Just (fromIntegral x)
  fromBsonValue (BsonInt64 x) = Just (fromEnum x)
  fromBsonValue (BsonFloat x) = Just (round x)
  fromBsonValue _ = Nothing

instance BsonVal Integer where
  toBsonValue n = maybe (maybe err BsonInt64 $ fitInt n)
                        BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for BsonInt{32,64}"
  fromBsonValue (BsonInt32 x) = Just (fromIntegral x)
  fromBsonValue (BsonInt64 x) = Just (fromIntegral x)
  fromBsonValue (BsonFloat x) = Just (round x)
  fromBsonValue _ = Nothing

--
-- To/From HsonValue
--
-- Lot of redundant instances to avoid use of gnarly GHC extensions
--

-- | Class used to (de)construct 'HsonValue's
class (Typeable a, Show a) => HsonVal a where
  -- | Convert to 'HsonValue'
  toHsonValue   :: a -> HsonValue
  -- | Convert from 'HsonValue'
  fromHsonValue :: HsonValue -> Maybe a

instance HsonVal Double where
  toHsonValue = HsonValue . BsonFloat
  fromHsonValue (HsonValue (BsonFloat x)) = Just x
  fromHsonValue (HsonValue (BsonInt32 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = Just (fromIntegral x)
  fromHsonValue _ = Nothing

instance HsonVal Float where
  toHsonValue = HsonValue . BsonFloat . realToFrac
  fromHsonValue (HsonValue (BsonFloat x)) = Just (realToFrac x)
  fromHsonValue (HsonValue (BsonInt32 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = Just (fromIntegral x)
  fromHsonValue _ = Nothing

instance HsonVal Text where
  toHsonValue = HsonValue . BsonString
  fromHsonValue (HsonValue (BsonString x)) = Just x
  fromHsonValue _ = Nothing

instance HsonVal String where
  toHsonValue = HsonValue . BsonString . T.pack
  fromHsonValue (HsonValue (BsonString x)) = Just $ T.unpack x
  fromHsonValue _ = Nothing

instance HsonVal BsonDocument where
  toHsonValue = HsonValue . BsonDoc
  fromHsonValue (HsonValue (BsonDoc x)) = Just x
  fromHsonValue _ = Nothing

instance HsonVal [BsonValue] where
  toHsonValue = HsonValue . BsonArray
  fromHsonValue (HsonValue (BsonArray x)) = Just x
  fromHsonValue _ = Nothing

instance (HsonVal a, BsonVal a) => HsonVal [a] where
  toHsonValue = HsonValue . BsonArray . map toBsonValue
  fromHsonValue (HsonValue (BsonArray x)) = mapM cast x
  fromHsonValue _ = Nothing

instance HsonVal Binary where
  toHsonValue = HsonValue . BsonBlob
  fromHsonValue (HsonValue (BsonBlob x)) = Just x
  fromHsonValue _ = Nothing


instance HsonVal ObjectId where
  toHsonValue = HsonValue . BsonObjId
  fromHsonValue (HsonValue (BsonObjId x)) = Just x
  fromHsonValue _ = Nothing

instance HsonVal Bool where
  toHsonValue = HsonValue . BsonBool
  fromHsonValue (HsonValue (BsonBool x)) = Just x
  fromHsonValue _ = Nothing

instance HsonVal UTCTime where
  toHsonValue = HsonValue . BsonUTC
  fromHsonValue (HsonValue (BsonUTC x)) = Just x
  fromHsonValue _ = Nothing

instance HsonVal (Maybe BsonValue) where
  toHsonValue Nothing  = HsonValue BsonNull
  toHsonValue (Just v) = HsonValue v
  fromHsonValue (HsonValue BsonNull) = Just Nothing
  fromHsonValue (HsonValue v)        = Just (Just v)
  fromHsonValue _ = Nothing

instance (HsonVal a, BsonVal a) => HsonVal (Maybe a) where
  toHsonValue Nothing  = HsonValue BsonNull
  toHsonValue (Just a) = HsonValue $ toBsonValue a
  fromHsonValue (HsonValue BsonNull) = Just Nothing
  fromHsonValue (HsonValue v)        = fmap Just (fromBsonValue v)
  fromHsonValue _ = Nothing

instance HsonVal Int32 where
  toHsonValue = HsonValue . BsonInt32
  fromHsonValue (HsonValue (BsonInt32 x)) = Just x
  fromHsonValue (HsonValue (BsonInt64 x)) = fitInt x
  fromHsonValue (HsonValue (BsonFloat x)) = Just (round x)
  fromHsonValue _ = Nothing

instance HsonVal Int64 where
  toHsonValue = HsonValue . BsonInt64
  fromHsonValue (HsonValue (BsonInt64 x)) = Just x
  fromHsonValue (HsonValue (BsonInt32 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = Just (round x)
  fromHsonValue _ = Nothing

instance HsonVal Int where
  toHsonValue n = HsonValue $ maybe (BsonInt64 $ fromIntegral n)
                                    BsonInt32 (fitInt n)
  fromHsonValue (HsonValue (BsonInt32 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = Just (fromEnum x)
  fromHsonValue (HsonValue (BsonFloat x)) = Just (round x)
  fromHsonValue _ = Nothing

instance HsonVal Integer where
  toHsonValue n = HsonValue $ maybe (maybe err BsonInt64 $ fitInt n)
                                    BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for HsonInt{32,64}"
  fromHsonValue (HsonValue (BsonInt32 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = Just (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = Just (round x)
  fromHsonValue _ = Nothing

instance HsonVal PolicyLabeled where
  toHsonValue   = HsonLabeled
  fromHsonValue (HsonLabeled v) = Just v
  fromHsonValue _               = Nothing

--
-- Helpers
--
  

-- | From "Data.Bson": Cast number to end type, if it \"fits\".
fitInt :: forall n m. (Integral n, Integral m, Bounded m) => n -> Maybe m
fitInt n = if fromIntegral (minBound :: m) <= n
              && n <= fromIntegral (maxBound :: m)
             then Just (fromIntegral n)
             else Nothing
