{-# LANGUAGE DeriveDataTypeable #-}

{- |

This code is based on "Data.Bson".


Invariants:

* Policy-labeled values are only allowed at the top-level. In other
  words, it is not allowed to have a policy-labeled policy-labeled
  value or an array of policy labeled values.
  The trusted code must retain this invariant.
-}

module Hails.Data.Hson.TCB where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int32, Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable
import qualified Data.Bson as Bson
import           Data.Bson ( ObjectId(..) )
import qualified Data.ByteString.Char8 as S8
import           Data.Serialize

import           LIO
import           LIO.Labeled.TCB (labelTCB, unlabelTCB)
import           LIO.DCLabel

infix 0 :=

type S8 = S8.ByteString

-- | A BSON @field@ is a named value, where the @fName@ is a string and
-- the @fValue@ is a BSON 'HsonValue'
--
-- > infix 0 :=
--
data Field = (:=) { fName :: !HsonName
                  -- ^ Name of field
                  , fValue :: HsonValue
                  -- ^ Value of field
                  }  deriving (Typeable, Eq, Ord)

-- | The name of a BSON field
type HsonName = Text

-- | A @HsonValue@ is a subset of BSON values.
data HsonValue = HsonFloat Double
               -- ^ Float value
               | HsonString  Text
               -- ^ String value
               | HsonDoc Document
               -- ^ Inner document
               | HsonArray [HsonValue]
               -- ^ List of values
               | HsonBin Binary
               -- ^ Binary value
               | HsonObjId ObjectId
               -- ^ Object Id value
               | HsonBool Bool
               -- ^ Boolean value
               | HsonUTC UTCTime
               -- ^ Time stamp value
               | HsonNull
               -- ^ The @NULL@ value
               | HsonInt32 Int32
               -- ^ 32-bit integer
               | HsonInt64 Int64
               -- ^ 64-bit integer
               | HsonLabeled PolicyLabeled
               -- ^ Policy labeled value
               deriving (Typeable, Eq, Ord)

-- | A @PolicyLabeled@ value can be either an unlabeled value for which
-- the policy needs to be applied (@NeedPolicyTCB@), or an already
-- labeled value (@HasPolicyTCB@). @PolicyLabeled@ is an opaque type;
-- code should not be able to inspect the value even if the policy has
-- not yet been applied.
data PolicyLabeled = NeedPolicyTCB HsonValue
                     -- ^ Policy was not applied 
                   | HasPolicyTCB (DCLabeled HsonValue)
                     -- ^ Policy applied
                   deriving (Typeable)
instance Eq PolicyLabeled  where (==) _ _ = False
instance Ord PolicyLabeled where (<=) _ _ = False


-- | Create a policy labeled value given an unlabeled 'HsonValue'.
needPolicy :: HsonValue -> PolicyLabeled
needPolicy = NeedPolicyTCB

-- | Create a policy labeled value a labeled 'HsonValue'.
hasPolicy :: DCLabeled HsonValue -> PolicyLabeled
hasPolicy = HasPolicyTCB

-- | A document is a list of 'Field's.
type Document = [Field]

-- | Arbitrary binary blob
newtype Binary = Binary { unBinary :: S8 }
  deriving (Typeable, Show, Read, Eq, Ord)

--
-- Convert to/from "Data.Bson"
--

-- | Convert a 'Field' to a Bson @Field@.
fieldToBsonFieldTCB :: Field -> Bson.Field
fieldToBsonFieldTCB f = (Bson.:=) (fName f) (hsonToBsonTCB $ fValue f)

-- | Convert a Bson @Field@ to a 'Field'.
bsonFieldToHsonFieldTCB :: Bson.Field -> Field
bsonFieldToHsonFieldTCB ((Bson.:=) n v) = n := (bsonToHsonTCB v)

-- | Convert a 'Document' to a Bson @Document@.
docToBsonDocTCB :: Document -> Bson.Document
docToBsonDocTCB = map fieldToBsonFieldTCB

-- | Convert a Bson @Document@.  to a 'Document'.
bsonDocToHsonDocTCB :: Bson.Document -> Document
bsonDocToHsonDocTCB = map bsonFieldToHsonFieldTCB

-- | Convert a 'HsonValue' to a Bson @Value@.
hsonToBsonTCB :: HsonValue -> Bson.Value
hsonToBsonTCB hv =
  case hv of
   (HsonFloat d)    -> Bson.Float d
   (HsonString t)   -> Bson.String t
   (HsonDoc d)      -> Bson.Doc $ docToBsonDocTCB d
   (HsonArray hs)   -> Bson.Array $ hsonToBsonTCB `map` hs
   (HsonBin b)      -> Bson.Bin . Bson.Binary . unBinary $ b
   (HsonObjId oid)  -> Bson.ObjId oid
   (HsonBool b)     -> Bson.Bool b
   (HsonUTC t)      -> Bson.UTC t
   HsonNull         -> Bson.Null         
   (HsonInt32 i)    -> Bson.Int32 i
   (HsonInt64 i)    -> Bson.Int64 i
   (HsonLabeled (HasPolicyTCB lv)) ->
       Bson.Doc $ docToBsonDocTCB $ 
          [ __hails_HsonLabeled_label := (HsonBin .
                                          Binary  .
                                          encode  .
                                          labelOf $ lv)
          , __hails_HsonLabeled_value := unlabelTCB lv ]
   (HsonLabeled (NeedPolicyTCB _)) ->
       error $ "BUG: hsonToBsonTCB: all policy labeled values"
                ++ " must have labeled values"


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

-- | Convert a 'HsonValue' to a Bson @Value@.
bsonToHsonTCB :: Bson.Value -> HsonValue
bsonToHsonTCB hv =
  case hv of
   (Bson.Float d)             -> HsonFloat d
   (Bson.String t)            -> HsonString t
   (Bson.Doc d)               ->
     let doc = bsonDocToHsonDocTCB d
     in maybe (HsonDoc doc) HsonLabeled $ maybePolicyLabeledTCB doc
   (Bson.Array hs)            -> HsonArray $ bsonToHsonTCB `map` hs
   (Bson.Bin (Bson.Binary b)) -> HsonBin . Binary $ b
   (Bson.ObjId oid)           -> HsonObjId oid
   (Bson.Bool b)              -> HsonBool b
   (Bson.UTC t)               -> HsonUTC t
   Bson.Null                  -> HsonNull         
   (Bson.Int32 i)             -> HsonInt32 i
   (Bson.Int64 i)             -> HsonInt64 i
   _                          -> 
       error $ "BUG: bsonToHsonTCB: cannot marshall all BSON types"

-- | Turn a hails-internal document to a policy labeled value.
maybePolicyLabeledTCB :: Document -> Maybe PolicyLabeled
maybePolicyLabeledTCB doc = do
  (HsonBin b) <- look __hails_HsonLabeled_label doc
  l <- decode' $ unBinary b
  v <- look __hails_HsonLabeled_value doc
  return . hasPolicy $ labelTCB l v
    where decode' b = case decode b of
                        Left _ -> Nothing
                        Right v -> v

-- | Retrieve the value of then named field from the document, or
-- 'fail' if not found.
look :: (Monad m) => HsonName -> Document -> m HsonValue
look k doc = maybe notFound (return . fValue) $ List.find ((k ==) . fName) doc
  where notFound = fail $ "look: Expected " ++ show k
                          -- TODO: ++ " in " ++ show doc

--
-- Helpers
--

-- | Check if document has nested policy-labeled values.
hasOnlyTopLevelPLsTCB :: Document -> Bool
hasOnlyTopLevelPLsTCB doc = hasOnlyTopLevelPLs' doc False


