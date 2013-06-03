
module Hails.Data.Hson.Instances (clean, sortDoc) where

import Data.List (sortBy, nubBy)
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import LIO.Instances ()
import Hails.Data.Hson
import Hails.Data.Hson.TCB

instance Arbitrary ObjectId where
    arbitrary = Oid <$> arbitrary <*> arbitrary

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary

instance Arbitrary BsonValue where
  arbitrary = oneof [ BsonFloat   <$> arbitrary
                    , BsonString  <$> arbitrary
                    , BsonDoc     <$> doc
                    , BsonArray   <$> arr
                    , BsonBlob    <$> arbitrary
                    , BsonObjId   <$> arbitrary
                    , BsonBool    <$> arbitrary
-- TODO: comment this out when serialization bug in bson package is fixed.
--                    , BsonUTC     <$> utc
                    , return BsonNull
                    , BsonInt32   <$> arbitrary
                    , BsonInt64   <$> arbitrary
                    ]
    where arr = sized $ \len -> take (min 3 len) <$>
                        (arbitrary :: Gen [BsonValue])
          doc = sized $ \len -> take (min 3 len) <$>
                        (arbitrary :: Gen [BsonField])
--          utc = (\u -> u { utctDayTime = 0 }) <$> arbitrary

instance Arbitrary BsonField where
    arbitrary = BsonField <$> n <*> arbitrary
      where n = oneof $ map (\x -> return . T.singleton $ x) ['A'..'Z']

instance Arbitrary PolicyLabeled where
  arbitrary = oneof [ NeedPolicyTCB <$> arbitrary
                    , HasPolicyTCB  <$> arbitrary ]


instance Arbitrary HsonValue where
  arbitrary = oneof [ HsonValue <$> arbitrary
                    , HsonLabeled <$> arbitrary ]

instance Arbitrary HsonField where
    arbitrary = HsonField <$> n <*> arbitrary
      where n = oneof $ map (\x -> return . T.singleton $ x) ['A'..'Z']

-- | Remove documents with same field name
clean :: (IsField f) => [f] -> [f]
clean = nubBy (\f1 f2 -> fieldName f1 == fieldName f2)

-- | Sort documents
sortDoc :: (IsField f) => [f] -> [f]
sortDoc = sortBy (\f1 f2 -> compare (fieldName f1)  (fieldName f2))
