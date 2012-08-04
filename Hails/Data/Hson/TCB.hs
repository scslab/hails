{-# LANGUAGE DeriveDataTypeable,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables
             #-}

{- |

This code is based on "Data.Bson".


-}

module Hails.Data.Hson.TCB () where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int32, Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable
import qualified Data.Bson as Bson
import qualified Data.Bson.Binary as Bson
import           Data.Bson ( ObjectId(..) )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Serialize
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary

import           LIO
import           LIO.Labeled.TCB (labelTCB, unlabelTCB)
import           LIO.DCLabel

infix 0 =:

type S8 = S8.ByteString




--
-- Document
--

-- | A top-level document containing 'HsonField's.
type HsonDocument = [HsonField]

-- | A (possibly top-)level document containing 'BsonField's.
type BsonDocument = [BsonField]

--
-- Fields
--

-- | The name of a BSON field
type HsonName = Text

-- | A field containing a named 'BsonValue'
data BsonField = BsonField !HsonName BsonValue
  deriving (Typeable, Eq, Ord)

instance Show BsonField where
  show (BsonField n v) = T.unpack n ++ " =: " ++ show v

-- | A field containing a named 'HsonValue'
data HsonField = HsonField !HsonName HsonValue
  deriving (Typeable, Eq, Ord)

instance Show HsonField where
  show (HsonField n v) = T.unpack n ++ " =: " ++ show v

-- | Class used to define fields.
class (Show v, Show f) => Field v f where
  -- | Given a name and value construct either a 'HsonField' or
  -- 'BsonField'
  (=:) :: HsonName -> v -> f

instance Field BsonValue BsonField where
  (=:) = BsonField
instance Field BsonValue HsonField where
  n =: v = n =: HsonValue v
instance Field HsonValue HsonField where
  (=:) = HsonField

--
-- Values
--

-- | A @BsonValue@ is a subset of BSON values.
data BsonValue = BsonFloat Double
               -- ^ Float value
               | BsonString Text
               -- ^ String value
               | BsonDoc BsonDocument
               -- ^ Inner document
               | BsonArray [BsonValue]
               -- ^ List of values
               | BsonBlob Binary
               -- ^ Binary blob value
               | BsonObjId ObjectId
               -- ^ Object Id value
               | BsonBool Bool
               -- ^ Boolean value
               | BsonUTC UTCTime
               -- ^ Time stamp value
               | BsonNull
               -- ^ The @NULL@ value
               | BsonInt32 Int32
               -- ^ 32-bit integer
               | BsonInt64 Int64
               -- ^ 64-bit integer
               deriving (Typeable, Eq, Ord)

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

-- | An @HsonValue@ is a top-level value that may either be a
-- 'BsonValue' or a policy labeled value. The separation of values
-- into 'BsonValue' and 'HsonValue' is solely due to the restriction
-- that policy-labeled values may only occur at the top level and
-- 'BsonValue's may be nested (see 'BsonArray' and 'BsonDoc').
data HsonValue = HsonValue BsonValue
                 -- ^ Bson value
               | HsonLabeled PolicyLabeled
                 -- ^ Policy labeled value
                 deriving (Typeable, Eq, Ord)

instance Show HsonValue where
  show (HsonValue   h) = show h
  show (HsonLabeled _) = "HsonLabeled"



--
-- Policy labeled values
--

-- | A @PolicyLabeled@ value can be either an unlabeled value for which
-- the policy needs to be applied (@NeedPolicyTCB@), or an already
-- labeled value (@HasPolicyTCB@). @PolicyLabeled@ is an opaque type;
-- code should not be able to inspect the value even if the policy has
-- not yet been applied.
data PolicyLabeled = NeedPolicyTCB BsonValue
                     -- ^ Policy was not applied 
                   | HasPolicyTCB (DCLabeled BsonValue)
                     -- ^ Policy applied
                   deriving (Typeable)
instance Eq PolicyLabeled  where (==) _ _ = False
instance Ord PolicyLabeled where (<=) _ _ = False
instance Show PolicyLabeled where show _ = "PolicyLabeled"


-- | Create a policy labeled value given an unlabeled 'HsonValue'.
needPolicy :: BsonValue-> PolicyLabeled
needPolicy = NeedPolicyTCB

-- | Create a policy labeled value a labeled 'HsonValue'.
hasPolicy :: DCLabeled BsonValue -> PolicyLabeled
hasPolicy = HasPolicyTCB

--
-- Binary values
--

-- | Arbitrary binary blob
newtype Binary = Binary { unBinary :: S8 }
  deriving (Typeable, Show, Read, Eq, Ord)


--
-- Convert to "Data.Bson"
--

-- | Convert 'HsonValue' to a "Data.Bson" @Value@.
hsonToDataBsonTCB :: HsonValue -> Bson.Value
hsonToDataBsonTCB (HsonValue b) = bsonToDataBsonTCB b
hsonToDataBsonTCB (HsonLabeled (HasPolicyTCB lv)) =
  toUserDef . hsonDocToDataBsonDocTCB $ 
     [ __hails_HsonLabeled_label =: (BsonBlob .
                                     Binary  .
                                     encode  .
                                     labelOf $ lv)
     , __hails_HsonLabeled_value =: unlabelTCB lv ]
    where toUserDef = Bson.UserDef
                    . Bson.UserDefined
                    . strictify
                    . Binary.runPut
                    . Bson.putDocument
          strictify = S8.concat . L.toChunks
hsonToDataBsonTCB _ =
  error $ "hsonToDataBsonTCB: all policy labeled values" ++
          " must have labeled values"

-- | Convert 'BsonValue' to a "Data.Bson" @Value@.
bsonToDataBsonTCB :: BsonValue -> Bson.Value
bsonToDataBsonTCB bv = case bv of
  (BsonFloat d)   -> Bson.Float d
  (BsonString t)  -> Bson.String t
  (BsonDoc d)     -> Bson.Doc $ bsonDocToDataBsonDocTCB d
  (BsonArray hs)  -> Bson.Array $ bsonToDataBsonTCB `map` hs
  (BsonBlob b)    -> Bson.Bin . Bson.Binary . unBinary $ b
  (BsonObjId oid) -> Bson.ObjId oid
  (BsonBool b)    -> Bson.Bool b
  (BsonUTC t)     -> Bson.UTC t
  BsonNull        -> Bson.Null         
  (BsonInt32 i)   -> Bson.Int32 i
  (BsonInt64 i)   -> Bson.Int64 i


-- | Convert a 'HsonField' to a Bson @Field@.
hsonFieldToDataBsonFieldTCB :: HsonField -> Bson.Field
hsonFieldToDataBsonFieldTCB (HsonField n v) =
  (Bson.:=) n (hsonToDataBsonTCB v)

-- | Convert a 'HsonDocument' to a Bson @Document@.
hsonDocToDataBsonDocTCB :: HsonDocument -> Bson.Document
hsonDocToDataBsonDocTCB = map hsonFieldToDataBsonFieldTCB

-- | Convert a 'BsonField' to a Bson @Field@.
bsonFieldToDataBsonFieldTCB :: BsonField -> Bson.Field
bsonFieldToDataBsonFieldTCB (BsonField n v) =
  (Bson.:=) n (bsonToDataBsonTCB v)

-- | Convert a 'BsonDocument' to a Bson @Document@.
bsonDocToDataBsonDocTCB :: BsonDocument -> Bson.Document
bsonDocToDataBsonDocTCB = map bsonFieldToDataBsonFieldTCB


--
-- Convert from "Data.Bson"
--

-- | Convert a "Data.Bson" @Field@ to 'BsonField'.
dataBsonFieldToBsonFieldTCB :: Bson.Field -> BsonField
dataBsonFieldToBsonFieldTCB ((Bson.:=) n v) = n =: (dataBsonToBsonTCB v)

-- | Convert a "Data.Bson" @Document@.  to a 'BsonDocument'.
dataBsonDocToBsonDocTCB :: Bson.Document -> BsonDocument
dataBsonDocToBsonDocTCB = map dataBsonFieldToBsonFieldTCB

-- | Convert 'BsonValue' to a "Data.Bson" @Value@.
dataBsonToBsonTCB :: Bson.Value -> BsonValue
dataBsonToBsonTCB bv = case bv of
  (Bson.Float d)   -> BsonFloat d
  (Bson.String t)  -> BsonString t
  (Bson.Doc d)     -> BsonDoc $ dataBsonDocToBsonDocTCB d
  (Bson.Array hs)  -> BsonArray $ dataBsonToBsonTCB `map` hs
  (Bson.Bin (Bson.Binary b))    -> BsonBlob . Binary $ b
  (Bson.ObjId oid) -> BsonObjId oid
  (Bson.Bool b)    -> BsonBool b
  (Bson.UTC t)     -> BsonUTC t
  Bson.Null        -> BsonNull         
  (Bson.Int32 i)   -> BsonInt32 i
  (Bson.Int64 i)   -> BsonInt64 i
  _                -> error "dataBsonToBsonTCB: only support subset of BSON"


dataBsonDocToHsonDocTCB :: Bson.Document -> HsonDocument
dataBsonDocToHsonDocTCB = map toHson
  where toHson ((Bson.:=) n (Bson.UserDef (Bson.UserDefined b))) = 
          let bdoc = Binary.runGet Bson.getDocument (lazyfy b)
          in case maybePolicyLabeledTCB bdoc of
               Nothing -> error "dataBsonDocToHsonDocTCB: Expected PolicyLabeled"
               Just lv -> n =: HsonLabeled lv
        toHson ((Bson.:=) n v) = n =: dataBsonToBsonTCB v
        lazyfy x = L8.fromChunks [x]



-- | Hails internal field name for a policy labeled value (label part)
-- (label part).
__hails_HsonLabeled_label :: HsonName
__hails_HsonLabeled_label = add__hails_prefix $ T.pack "HsonLabeled_label"

-- | Hails internal field name for a policy labeled value (label part)
-- (name part).
__hails_HsonLabeled_value :: HsonName
__hails_HsonLabeled_value = add__hails_prefix $ T.pack "HsonLabeled_value"

-- | Hails internal prefix that is used to serialized labeled values.
add__hails_prefix :: Text -> Text
add__hails_prefix t = T.pack "__hails_" `T.append` t


-- | Turn a hails-internal document to a policy labeled value.
maybePolicyLabeledTCB :: Bson.Document -> Maybe PolicyLabeled
maybePolicyLabeledTCB doc = do
  (Bson.Binary b) <- Bson.lookup __hails_HsonLabeled_label doc
  l <- decode' b
  v <- Bson.look __hails_HsonLabeled_value doc
  return . hasPolicy $ labelTCB l (dataBsonToBsonTCB v)
    where decode' b = case decode b of
                        Left _ -> Nothing
                        Right v -> v

look :: Monad m => HsonName -> BsonDocument -> m BsonValue
look n doc = case List.find f doc of
               Just (BsonField _ v) -> return v
               _                    -> fail $ "look: Not found " ++ show n
  where f (BsonField n' _) = n == n'

--
-- Helpers
--

-- | Convert 'BsonDocument' to 'HsonDocument'
bsonDocToHsonDoc :: BsonDocument -> HsonDocument
bsonDocToHsonDoc = map bsonFieldToHsonField 

-- | Convert 'BsonField' to 'HsonField'
bsonFieldToHsonField :: BsonField -> HsonField
bsonFieldToHsonField (BsonField n v) = n =: v

-------------------------------------------------------------------------------
-- "Friendly" interface -------------------------------------------------------
-------------------------------------------------------------------------------

{-
instance BsonVal v => Field v BsonField where
  n =: v = n =: toBsonValue v
instance HsonVal v => Field v HsonField where
  n =: v = n =: toHsonValue v
-}

--
-- To/From BsonValue
--

class (Typeable a, Show a) => BsonVal a where
  toBsonValue   :: a -> BsonValue
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
-- Lot of redundant instances to avoid unecessary extensions

class (Typeable a, Show a) => HsonVal a where
  toHsonValue   :: a -> HsonValue
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
  toHsonValue n = HsonValue $ maybe (BsonInt64 $ fromIntegral n) BsonInt32 (fitInt n)
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
