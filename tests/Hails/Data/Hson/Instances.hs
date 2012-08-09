
module Hails.Data.Hson.Instances () where

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
                    , BsonUTC     <$> arbitrary
                    , return BsonNull ]
    where arr = sized $ \len -> take (min 3 len) <$>
                        (arbitrary :: Gen [BsonValue])
          doc = sized $ \len -> take (min 3 len) <$>
                        (arbitrary :: Gen [BsonField])

instance Arbitrary BsonField where
    arbitrary = BsonField <$> arbitrary <*> arbitrary


instance Arbitrary PolicyLabeled where
  arbitrary = oneof [ NeedPolicyTCB <$> arbitrary
                    , HasPolicyTCB  <$> arbitrary ]


instance Arbitrary HsonValue where
  arbitrary = oneof [ HsonValue <$> arbitrary
                    , HsonLabeled <$> arbitrary ]

instance Arbitrary HsonField where
    arbitrary = HsonField <$> arbitrary <*> arbitrary
