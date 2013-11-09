{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |


This module exports the type for a Hails BSON document, 'HsonDoc'.  A
Hails document is akin to "Data.Bson"\'s documents, but differs in two
ways. First, Hails restricts the number of types to a subset of BSON's
(see 'BsonVal'). This restriction is primarily due to the fact that
many of the BSON types are redundant and not used (at least within
Hails). Second, Hails allows for documents to contain policy-labeled
values.

Policy labeled values ('PolicyLabeled') are permitted only at the
\"top-level\" of a document. (This is primarily done to keep
policy-specification simple and may change in the future.)
Consequently to allow for nested documents and documents containing an
array of values we separate top-level fields ('HsonField'), that may
contain policy labeled values, from potentially-nested fields
('BsonField'). A top-level field 'HsonField' is thus either a
'BsonField' or a 'PolicyLabled' value.

To keep the TCB compact, this module does not export the combinators
used to create documents in a friendly fashion. See "Hails.Data.Hson"
for the safe external API.


/Credit:/ Much of this code is based on/reuses "Data.Bson".
-}

module Hails.Data.Hson.TCB (
  -- * Documents
    HsonDocument, BsonDocument
  -- * Fields
  , FieldName, HsonField(..), BsonField(..)
  -- * Values
  , HsonValue(..), BsonValue(..)
  , PolicyLabeled(..), ObjectId(..), Binary(..), S8
  -- * Marshall to/from "Data.Bson"
  , hsonDocToDataBsonDocTCB 
  , dataBsonDocToHsonDocTCB 
  , bsonDocToDataBsonDocTCB 
  , dataBsonValueToHsonValueTCB 
  -- * Internal
  , add__hails_prefix 
  ) where

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
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import Control.Applicative

import           LIO.Labeled
import           LIO.TCB

import System.IO.Unsafe
import LIO.RCLabel
import LIO.SafeCopy
import Hails.Copy

-- | Strict ByeString
type S8 = S8.ByteString




--
-- Document
--

-- | A top-level document containing 'HsonField's.
type HsonDocument = [HsonField]

trHsonDocument :: Transfer HsonDocument
trHsonDocument = trList 200 trHsonField

-- | A (possibly top-)level document containing 'BsonField's.
type BsonDocument = [BsonField]

trBsonDocument = trList 500 trBsonField

--
-- Fields
--

-- | The name of a field.
type FieldName = Text

-- | A field containing a named 'BsonValue'
data BsonField = BsonField !FieldName BsonValue
  deriving (Typeable, Eq, Ord)

trBsonField = Transfer $ \(BsonField a b) -> BsonField <$> transfer trText a <*> transfer trBsonValue b

-- | A field containing a named 'HsonValue'
data HsonField = HsonField !FieldName HsonValue
  deriving (Typeable, Eq, Ord)

trHsonField :: Transfer HsonField
trHsonField = Transfer $ \(HsonField n v) -> HsonField <$> transfer trText n <*> transfer trHsonValue v

--
-- Values
--

-- | A @BsonValue@ is a subset of BSON ("Data.Bson") values.  Note that a
-- @BsonValue@ cannot contain any labeled values; all labeled values
-- occur in a document as 'HsonValue's.  Correspondingly, @BsonValue@s
-- may be arbitrarily nested.
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

trBsonValue :: Transfer BsonValue
trBsonValue = Transfer f
    where f (BsonFloat a) = BsonFloat <$> transfer trPrim a
          f (BsonString a) = BsonString <$> transfer trText a
          f (BsonDoc a) = BsonDoc <$> transfer trBsonDocument a
          f (BsonArray a) = BsonArray <$> transfer (trList 500 trBsonValue) a
          f (BsonBlob a) = BsonBlob <$> transfer trBinary a
          f (BsonObjId a) = BsonObjId <$> transfer trObjectId a
          f (BsonBool a) = BsonBool <$> transfer trPrim a
          f (BsonUTC a) = BsonUTC <$> transfer trUTCTime a
          f BsonNull = return BsonNull
          f (BsonInt32 a) = BsonInt32 <$> transfer trPrim a
          f (BsonInt64 a) = BsonInt64 <$> transfer trPrim a

trBinary :: Transfer Binary
trBinary = Transfer $ \(Binary b) -> Binary <$> transfer trS8ByteString b

-- | An @HsonValue@ is a top-level value that may either be a
-- 'BsonValue' or a policy labeled value. The separation of values
-- into 'BsonValue' and 'HsonValue' is solely due to the restriction
-- that policy-labeled values may only occur at the top level and
-- 'BsonValue's may be nested (e.g. using 'BsonArray' and 'BsonDoc').
data HsonValue = HsonValue BsonValue
                 -- ^ Bson value
               | HsonLabeled PolicyLabeled
                 -- ^ Policy labeled value
                 deriving (Typeable, Eq, Ord)

trHsonValue :: Transfer HsonValue
trHsonValue = Transfer f
    where f (HsonValue b) = HsonValue <$> transfer trBsonValue b
          f (HsonLabeled l) = HsonLabeled <$> transfer trPolicyLabeled l

-- | A @PolicyLabeled@ value can be either an unlabeled value for which
-- the policy needs to be applied (@NeedPolicyTCB@), or an already
-- labeled value (@HasPolicyTCB@). @PolicyLabeled@ is a partially-opaque
-- type; code should not be able to inspect the value of an unlabeleda
-- value, but may inspect an already labeled value.
data PolicyLabeled = NeedPolicyTCB BsonValue
                     -- ^ Policy was not applied 
                   | HasPolicyTCB (DCLabeled BsonValue)
                     -- ^ Policy applied
                   deriving (Typeable)

trPolicyLabeled :: Transfer PolicyLabeled
trPolicyLabeled = Transfer f
    where f (NeedPolicyTCB b) = NeedPolicyTCB <$> transfer trBsonValue b
          f x = copy x -- NB: fall through!!!! XXX maybe not enough

instance Eq PolicyLabeled   where (==) _ _ = True
instance Ord PolicyLabeled  where (<=) _ _ = False
instance Show PolicyLabeled where show _   = "PolicyLabeled"


-- | Arbitrary binary blob
newtype Binary = Binary { unBinary :: S8 }
  deriving (Typeable, Show, Read, Eq, Ord)


--
-- Convert to "Data.Bson"
--

-- | Convert 'HsonValue' to a "Data.Bson" @Value@. Note that
-- 'PolicyLabeled' values are marshalled out as "Data.Bson" @UserDefined@
-- values. This means that the @UserDefined@ type is reserved and
-- exposing it as a type in 'BsonValue' would potentially lead to leaks.
-- Note that the label is /NOT/ serialized, only the value. Hence,
-- after marshalling such that back it is important that a policy is 
-- applied to label the field.
hsonToDataBsonTCB :: HsonValue -> Bson.Value
hsonToDataBsonTCB (HsonValue b) = bsonToDataBsonTCB b
hsonToDataBsonTCB (HsonLabeled (HasPolicyTCB (LabeledTCB _ lv))) =
  toUserDef . hsonDocToDataBsonDocTCB $ 
     [ HsonField __hails_HsonLabeled_value $
            HsonValue (unsafePerformIO (maybe (error "was dead") return =<< readMultiRCRef lv)) ]
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


-- | Convert an 'HsonField' to a "Data.Bson" @Field@.
hsonFieldToDataBsonFieldTCB :: HsonField -> Bson.Field
hsonFieldToDataBsonFieldTCB (HsonField n v) =
  (Bson.:=) n (hsonToDataBsonTCB v)

-- | Convert a top-level document (i.e., 'HsonDocument') to a "Data.Bson"
-- @Document@. This is the primary marshall-out function.  All
-- 'PolicyLabeled' values are marshalled out as "Data.Bson" @UserDefined@
-- values. This means that the @UserDefined@ type is reserved and
-- exposing it as a type in 'BsonValue' would potentially lead to
-- vulnerabilities in which labeled values can be marshalled in from
-- well-crafted ByteStrings. Moreover, untrusted code should not have
-- access to this function; having such access would allow it to
-- inspect the serialized labeled values and thus violate IFC.
hsonDocToDataBsonDocTCB :: HsonDocument -> Bson.Document
hsonDocToDataBsonDocTCB = map hsonFieldToDataBsonFieldTCB

-- | Convert a 'BsonField' to a "Data.Bson" @Field@.
bsonFieldToDataBsonFieldTCB :: BsonField -> Bson.Field
bsonFieldToDataBsonFieldTCB (BsonField n v) =
  (Bson.:=) n (bsonToDataBsonTCB v)

-- | Convert a 'BsonDocument' to a "Data.Bson" @Document@.
bsonDocToDataBsonDocTCB :: BsonDocument -> Bson.Document
bsonDocToDataBsonDocTCB = map bsonFieldToDataBsonFieldTCB


--
-- Convert from "Data.Bson"
--

-- | Convert a "Data.Bson" @Field@ to 'BsonField'.
dataBsonFieldToBsonFieldTCB :: Bson.Field -> BsonField
dataBsonFieldToBsonFieldTCB ((Bson.:=) n v) = BsonField n (dataBsonToBsonTCB v)

-- | Convert a "Data.Bson" @Document@  to a 'BsonDocument'.
dataBsonDocToBsonDocTCB :: Bson.Document -> BsonDocument
dataBsonDocToBsonDocTCB = map dataBsonFieldToBsonFieldTCB

-- | Convert "Data.Bson" @Value@ to a 'BsonValue'.
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


-- | Convert "Data.Bson" @Document@ to a 'HsonDocument'. This is the
-- top-level function that marshalls BSON documents to Hails
-- documents. This function assumes that all documents have been
-- marshalled out using 'hsonDocToDataBsonDocTCB'. Otherwise, the
-- 'PolicyLabled' values that are created from the document may be
-- forged.
dataBsonDocToHsonDocTCB :: Bson.Document -> HsonDocument
dataBsonDocToHsonDocTCB =
  map (\((Bson.:=) n bv) -> HsonField n $ dataBsonValueToHsonValueTCB bv)

-- |Convert a "Data.Bson" @Value@ to a 'HsonValue'. See
-- 'dataBsonDocToHsonDocTCB'.
dataBsonValueToHsonValueTCB :: Bson.Value -> HsonValue
dataBsonValueToHsonValueTCB bv = case bv of
    (Bson.UserDef (Bson.UserDefined b)) ->
          let bdoc = Binary.runGet Bson.getDocument (lazyfy b)
          in case maybePolicyLabeledTCB bdoc of
               Nothing -> error $ "dataBsonValueToHsonValueTCB: "
                                ++ "Expected PolicyLabeled"
               Just lv -> HsonLabeled lv
    v -> HsonValue $ dataBsonToBsonTCB v
  where lazyfy x = L8.fromChunks [x]



-- | Hails internal field name for a policy labeled value (label part)
-- (name part).
__hails_HsonLabeled_value :: FieldName
__hails_HsonLabeled_value = add__hails_prefix $ T.pack "HsonLabeled_value"

-- | Hails internal prefix that is used to serialized labeled values.
add__hails_prefix :: FieldName -> FieldName
add__hails_prefix t = T.pack "__hails_" `T.append` t


-- | Convert a "Data.Bson" @Document@ to a policy labeled value.
maybePolicyLabeledTCB :: Bson.Document -> Maybe PolicyLabeled
maybePolicyLabeledTCB doc = do
  v <- Bson.look __hails_HsonLabeled_value doc
  return . NeedPolicyTCB $ dataBsonToBsonTCB v
