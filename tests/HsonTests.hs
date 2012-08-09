
module HsonTests (tests) where

import Data.List (sortBy, nubBy)
import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime(..))

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Hails.Data.Hson
import Hails.Data.Hson.Instances ()

tests :: [Test]
tests = [ toFromHsonValue
        , testDocOps 
        ]


--
-- Test HsonVal class
--

testHsonVal :: (HsonVal a, Eq a) => a -> Bool
testHsonVal a = case fromHsonValue . toHsonValue $ a of
    Nothing -> False
    Just a' -> a == a'

toFromHsonValue :: Test
toFromHsonValue = testGroup "To/from HsonValue"
    [ testProperty "Bool"          (testHsonVal :: Bool -> Bool)
    , testProperty "Double"        (testHsonVal :: Double -> Bool)
    , testProperty "Float"         (testHsonVal :: Float -> Bool)
    , testProperty "Int"           (testHsonVal :: Int -> Bool)
    , testProperty "Int32"         (testHsonVal :: Int32 -> Bool)
    , testProperty "Int64"         (testHsonVal :: Int64 -> Bool)
    , testProperty "Integer"       (testHsonVal :: Integer -> Bool)
    , testProperty "String"        (testHsonVal :: String -> Bool)
    , testProperty "UTCTime"       (testHsonVal :: UTCTime -> Bool)
    , testProperty "ObjectId"      (testHsonVal :: ObjectId -> Bool)
    , testProperty "Binary"        (testHsonVal :: Binary -> Bool)
    , testProperty "Text"          (testHsonVal :: Text -> Bool)
    , testProperty "BsonDocument"  (testHsonVal :: BsonDocument -> Bool)
    , testProperty "BsonValue"     (testHsonVal :: BsonValue -> Bool)
    , testProperty "[BsonValue]"   (testHsonVal :: [BsonValue] -> Bool)
    , testProperty "PolicyLabeled" (testHsonVal :: PolicyLabeled -> Bool)
    ]

          
--
-- Test include, exclude and merge
--

testDocOps :: Test
testDocOps = testGroup "Document operations"
  [ testProperty "Include Bson"          (testInclude :: BsonDocument -> BsonDocument -> Bool)
  , testProperty "Include Hson"          (testInclude :: HsonDocument -> HsonDocument -> Bool)
  , testProperty "Exclude Bson"          (testExclude :: BsonDocument -> BsonDocument -> Bool)
  , testProperty "Exclude Hson"          (testExclude :: HsonDocument -> HsonDocument -> Bool)
  , testProperty "Merge Bson"            (testMerge :: BsonDocument -> BsonDocument -> Bool)
  , testProperty "Merge Hson"            (testMerge :: HsonDocument -> HsonDocument -> Bool)
  , testProperty "Merge idempotent Bson" (propMergeIdempotent :: BsonDocument -> BsonDocument -> Bool)
  , testProperty "Merge idempotent Hson" (propMergeIdempotent :: HsonDocument -> HsonDocument -> Bool)
  ]


-- | Test include
testInclude :: (IsField f, Eq f) => [f] -> [f] -> Bool
testInclude d1 d2 =
  let doc1 = doSort . clean $ d1
      fs1  = map fieldName doc1
      doc2 = doSort . filter (\f -> fieldName f `notElem` fs1) . clean $ d2
      fs2  = map fieldName doc2
      doc  = doc1 ++ doc2
  in doSort (include fs1 doc) == doc1
  && doSort (include fs2 doc) == doc2

-- | Remove documents with same field name
clean :: (IsField f) => [f] -> [f]
clean = nubBy (\f1 f2 -> fieldName f1 == fieldName f2)

-- | Sort documents
doSort :: (IsField f) => [f] -> [f]
doSort = sortBy (\f1 f2 -> compare (fieldName f1)  (fieldName f2))

  
-- | Test exclude
testExclude :: (IsField f, Eq f) => [f] -> [f] -> Bool
testExclude d1 d2 =
  let doc1 = doSort . clean $ d1
      fs1  = map fieldName doc1
      doc2 = doSort . filter (\f -> fieldName f `notElem` fs1) . clean $ d2
      fs2  = map fieldName doc2
      doc  = doc1 ++ doc2
  in doSort (exclude fs1 doc) == doc2
  && doSort (exclude fs2 doc) == doc1

-- | Test merge
testMerge :: (Show f, IsField f, Eq f) => [f] -> [f] -> Bool
testMerge d1 d2 =
  let doc1 = doSort . clean $ d1
      fs1  = map fieldName doc1
      doc2 = doSort . clean $ d2
      doc2_nub = doSort . filter (\f -> fieldName f `notElem` fs1) $ doc2
  in doSort (merge doc1 doc2) == doSort (merge doc1 doc2_nub)

-- | Merge applied to document twice returns same thing
propMergeIdempotent :: (Show f, IsField f, Eq f) => [f] -> [f] -> Bool
propMergeIdempotent doc1 doc2 = 
  let m1 = merge doc1 doc2
      m2 = merge m1 doc2
  in m1 == m2
  && (merge doc1 doc1 == doc1)
  && (merge doc2 doc2 == doc2)
