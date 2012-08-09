{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts,
             DeriveDataTypeable,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             OverlappingInstances,
             FunctionalDependencies
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
  -- ** Operations on documents
  , DocOps(..)
  , DocValOps(..)
  , include, exclude
  , merge
  -- ** Converting to/from Hson/Bson
  , bsonDocToHsonDoc, bsonFieldToHsonField
  , isBsonDoc
  , hsonDocToBsonDoc
  , hsonDocToBsonDocStrict
  -- * Fields
  , FieldName, HsonField(..), BsonField(..)
  , IsField(..), Field(..), GenField(..)
  -- * Values
  , HsonValue(..), HsonVal(..)
  , BsonValue(..), BsonVal(..)
  , PolicyLabeled, needPolicy, hasPolicy
  , Binary(..)
  , ObjectId(..), genObjectId
  ) where

import           Prelude hiding (lookup)
import           Data.Maybe (mapMaybe)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int32, Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable
import           Data.Functor.Identity (runIdentity)
import qualified Data.Bson as Bson

import           LIO
import           LIO.DCLabel
import           LIO.TCB (ioTCB)

import           Hails.Data.Hson.TCB

infix 0 =:, -:


--
-- Fields
--

instance Show BsonField where
  show (BsonField n v) = T.unpack n ++ " -: " ++ show v

instance Show HsonField where
  show (HsonField n v) = T.unpack n ++ " -: " ++ show v

-- | Class for retrieving the name of a field.
class IsField f where
  -- | Get the name of a field.
  fieldName :: f -> FieldName

instance IsField BsonField where fieldName (BsonField n _) = n
instance IsField HsonField where fieldName (HsonField n _) = n

-- | Class used to define fields.
class (IsField f, Show v, Show f) => Field v f where
  -- | Given a name and value construct either a 'HsonField' or
  -- 'BsonField'
  (=:) :: FieldName -> v -> f
  -- | Get the field value.
  fieldValue :: Monad m => f -> m v

instance Field BsonValue BsonField where
  (=:) = BsonField
  fieldValue (BsonField _ v) = return v
instance Field BsonValue HsonField where
  n =: v = n =: HsonValue v
  fieldValue (HsonField _ (HsonValue v)) = return v
  fieldValue _ = fail "fieldValue: cannot extract PolicyLabled"
instance Field HsonValue HsonField where
  (=:) = HsonField
  fieldValue (HsonField _ v) = return v


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
-- Document operations
--

-- | Class used to implement operatoins on documents that return
-- 'HsonValue's or 'BsonValue's. The main role of this function is to
-- impose the functional dependency between values and fields.  As a
-- consequence 'look'ing up and getting 'valueAt' in a 'HsonDocument'
-- (resp. 'BsonDocument') will return a 'HsonValue' (resp. 'BsonValue').
-- This eliminates the need to specify the end type of very query, but
-- forces the programmer to cast between Hson and Bson values.
class Field v f => DocOps v f | v -> f, f -> v where
  -- | Find value of field in document, or fail not found.
  look :: (Field v f, Monad m) => FieldName -> [f] -> m v
  look n doc = case List.find ((==n) . fieldName) doc of
                 Just v -> fieldValue v
                 _      -> fail $ "look: Not found " ++ show n

  -- | Same as 'look', but 'fail's if the value is not found.
  valueAt :: Field v f => FieldName -> [f] -> v
  valueAt n = runIdentity . look n

instance DocOps HsonValue HsonField where
instance DocOps BsonValue BsonField where

-- | Only include fields specified.
include :: IsField f => [FieldName] -> [f] -> [f]
include ns doc = mapMaybe (\n -> List.find ((==n) . fieldName) doc) ns

-- | Exclude fields specified.
exclude :: IsField f => [FieldName] -> [f] -> [f]
exclude ns doc = filter ((\n -> notElem n ns) . fieldName) doc

-- | Merge documents with preference given to first one when both have
-- the same field name.
merge :: IsField f => [f] -> [f] -> [f]
merge doc1 doc2 = 
  let ns1 = map fieldName doc1
      doc2' = List.filter ((\n -> n `notElem` ns1) . fieldName) doc2
  in doc1 ++ doc2'

-- | Class used to implement operations on documents that return
-- Haskell values (as opposed to 'HsonValue' or 'BsonValue').
class DocValOps d v where
  -- | Same as 'look', but returns \"unwrapped\" value.
  lookup :: (Monad m) => FieldName -> d -> m v
  -- | Same as 'valueAt', but returns \"unwrapped\" value.
  at :: FieldName -> d -> v
  at n = runIdentity . lookup n

instance HsonVal v => DocValOps HsonDocument v where
  lookup n doc = look n doc >>= fromHsonValue

instance BsonVal v => DocValOps BsonDocument v where
  lookup n doc = look n doc >>= fromBsonValue


--
-- Converters
--

-- | Returns true if the document is composed solely of 'BsonValue's.
-- This function is useful when converting from 'HsonDocument' to
-- 'BsonDocument'.
isBsonDoc :: HsonDocument -> Bool
isBsonDoc doc = any f doc
  where f (HsonField _ (HsonLabeled _)) = False
        f _ = True

-- | Convert 'BsonDocument' to 'HsonDocument'
bsonDocToHsonDoc :: BsonDocument -> HsonDocument
bsonDocToHsonDoc = map bsonFieldToHsonField 

-- | Convert 'BsonField' to 'HsonField'
bsonFieldToHsonField :: BsonField -> HsonField
bsonFieldToHsonField (BsonField n v) = n =: v

-- | This is a relaxed version of 'hsonDocToBsonDocStrict' that only
-- converts fields containing 'BsonValue's. In other words, the
-- 'PolicyLabeled' values are dropped.
hsonDocToBsonDoc :: HsonDocument -> BsonDocument
hsonDocToBsonDoc doc = mapMaybe hsonFieldToBsonField doc

-- | Convert an 'HsonDocument' to a 'BsonDocument'. If any of the
-- fields contain 'PolicyLabeled' values (i.e., are 'HsonLabeled'
-- values) this function 'fail's, otherwise it returns the converted
-- document. To check for failure use 'isBsonDoc'.
hsonDocToBsonDocStrict :: Monad m => HsonDocument -> m BsonDocument
hsonDocToBsonDocStrict doc = mapM hsonFieldToBsonField doc

-- | Convert 'HsonField' to 'BsonField'
hsonFieldToBsonField :: Monad m => HsonField -> m BsonField
hsonFieldToBsonField (HsonField n (HsonValue v)) = return (n =: v)
hsonFieldToBsonField (HsonField n _) = 
  fail $ "hsonFieldToBsonField: field " ++ show n ++ " is PolicyLabeled"

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
  fromBsonValue :: Monad m => BsonValue -> m a

instance BsonVal Double where
  toBsonValue = BsonFloat
  fromBsonValue (BsonFloat x) = return x
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Float where
  toBsonValue = BsonFloat . realToFrac
  fromBsonValue (BsonFloat x) = return (realToFrac x)
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Text where
  toBsonValue = BsonString
  fromBsonValue (BsonString x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal String where
  toBsonValue = BsonString . T.pack
  fromBsonValue (BsonString x) = return $ T.unpack x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal BsonDocument where
  toBsonValue = BsonDoc
  fromBsonValue (BsonDoc x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal [BsonValue] where
  toBsonValue = BsonArray
  fromBsonValue (BsonArray x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance (BsonVal a) => BsonVal [a] where
  toBsonValue = BsonArray . map toBsonValue
  fromBsonValue (BsonArray x) = mapM castM x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Binary where
  toBsonValue = BsonBlob
  fromBsonValue (BsonBlob x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"


instance BsonVal ObjectId where
  toBsonValue = BsonObjId
  fromBsonValue (BsonObjId x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Bool where
  toBsonValue = BsonBool
  fromBsonValue (BsonBool x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal UTCTime where
  toBsonValue = BsonUTC
  fromBsonValue (BsonUTC x) = return x
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal (Maybe BsonValue) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just v) = v
  fromBsonValue BsonNull = return Nothing
  fromBsonValue v        = return (Just v)

instance (BsonVal a) => BsonVal (Maybe a) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just a) = toBsonValue a
  fromBsonValue BsonNull = return Nothing
  fromBsonValue v        = fromBsonValue v

instance BsonVal Int32 where
  toBsonValue = BsonInt32
  fromBsonValue (BsonInt32 x) = return x
  fromBsonValue (BsonInt64 x) = fitInt x
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Int64 where
  toBsonValue = BsonInt64
  fromBsonValue (BsonInt64 x) = return x
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Int where
  toBsonValue n = maybe (BsonInt64 $ fromIntegral n) BsonInt32 (fitInt n)
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromEnum x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

instance BsonVal Integer where
  toBsonValue n = maybe (maybe err BsonInt64 $ fitInt n)
                        BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for BsonInt{32,64}"
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue _ = fail "fromBsonValue: no conversion"

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
  fromHsonValue :: Monad m => HsonValue -> m a

instance HsonVal Double where
  toHsonValue = HsonValue . BsonFloat
  fromHsonValue (HsonValue (BsonFloat x)) = return x
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Float where
  toHsonValue = HsonValue . BsonFloat . realToFrac
  fromHsonValue (HsonValue (BsonFloat x)) = return (realToFrac x)
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Text where
  toHsonValue = HsonValue . BsonString
  fromHsonValue (HsonValue (BsonString x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal String where
  toHsonValue = HsonValue . BsonString . T.pack
  fromHsonValue (HsonValue (BsonString x)) = return $ T.unpack x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal BsonDocument where
  toHsonValue = HsonValue . BsonDoc
  fromHsonValue (HsonValue (BsonDoc x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal [BsonValue] where
  toHsonValue = HsonValue . BsonArray
  fromHsonValue (HsonValue (BsonArray x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance (HsonVal a, BsonVal a) => HsonVal [a] where
  toHsonValue = HsonValue . BsonArray . map toBsonValue
  fromHsonValue (HsonValue (BsonArray x)) = mapM castM x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Binary where
  toHsonValue = HsonValue . BsonBlob
  fromHsonValue (HsonValue (BsonBlob x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal ObjectId where
  toHsonValue = HsonValue . BsonObjId
  fromHsonValue (HsonValue (BsonObjId x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Bool where
  toHsonValue = HsonValue . BsonBool
  fromHsonValue (HsonValue (BsonBool x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal UTCTime where
  toHsonValue = HsonValue . BsonUTC
  fromHsonValue (HsonValue (BsonUTC x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal (Maybe BsonValue) where
  toHsonValue Nothing = HsonValue BsonNull
  toHsonValue (Just v) = HsonValue v
  fromHsonValue (HsonValue BsonNull) = return Nothing
  fromHsonValue (HsonValue v)        = return (Just v)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance (HsonVal a, BsonVal a) => HsonVal (Maybe a) where
  toHsonValue Nothing  = HsonValue BsonNull
  toHsonValue (Just a) = HsonValue $ toBsonValue a
  fromHsonValue (HsonValue BsonNull) = return Nothing
  fromHsonValue (HsonValue v)        = fromBsonValue v
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int32 where
  toHsonValue = HsonValue . BsonInt32
  fromHsonValue (HsonValue (BsonInt32 x)) = return x
  fromHsonValue (HsonValue (BsonInt64 x)) = fitInt x
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int64 where
  toHsonValue = HsonValue . BsonInt64
  fromHsonValue (HsonValue (BsonInt64 x)) = return x
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int where
  toHsonValue n = HsonValue $ maybe (BsonInt64 $ fromIntegral n)
                                    BsonInt32 (fitInt n)
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromEnum x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Integer where
  toHsonValue n = HsonValue $ maybe (maybe err BsonInt64 $ fitInt n)
                                    BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for HsonInt{32,64}"
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal BsonValue where
  toHsonValue = HsonValue
  fromHsonValue (HsonValue v) = return v
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal PolicyLabeled where
  toHsonValue   = HsonLabeled
  fromHsonValue (HsonLabeled v) = return v
  fromHsonValue _ = fail "fromHsonValue: no conversion"


--
-- ObjectId
--

-- | Create a fresh ObjectId.
genObjectId :: MonadDC m => m ObjectId
genObjectId = liftLIO $ ioTCB Bson.genObjectId

--
-- Helpers
--
  

-- | From "Data.Bson": Cast number to end type, if it \"fits\".
fitInt :: forall a b m. (Integral a, Integral b, Bounded b, Monad m)
        => a -> m b
fitInt n = if fromIntegral (minBound :: b) <= n
              && n <= fromIntegral (maxBound :: b)
             then return $ fromIntegral n
             else fail "fitInt: number is too big"

-- | Generic monad version of 'cast'.
castM :: (Typeable a, Typeable b, Monad m) => a -> m b
castM x = case cast x of
            Just v -> return v
            _      -> fail "castM: cast failed"
