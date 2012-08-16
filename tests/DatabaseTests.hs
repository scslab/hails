{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables,
             DeriveDataTypeable #-}

module DatabaseTests where

import Prelude hiding (lookup)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q


import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text ()

import qualified Database.MongoDB as Mongo

import Control.Monad
import Control.Exception

import Data.Typeable

import LIO
import LIO.Labeled.TCB
import LIO.TCB (ioTCB, catchTCB)
import LIO.Privs.TCB (mintTCB)
import LIO.DCLabel
import LIO.DCLabel.Instances ()

import Hails.Data.Hson
import Hails.Data.Hson.TCB
import Hails.Data.Hson.Instances
import Hails.PolicyModule
import Hails.PolicyModule.TCB
import Hails.Database.Core
import Hails.Database.TCB
import Hails.Database.Query

import System.Posix.Env (setEnv)
import System.IO.Unsafe

tests :: [Test]
tests = [
    testGroup "withPolicy tests" [
        testCase "Succeed loading existant policy module"
                 test_withPolicyModuleP_ok
      , testCase "Fail loading non-existant policy module"
                 test_withPolicyModuleP_fail
    ]
  , testGroup "Label database/collection-set tests" [
      testProperty "Can label database with bounded label"
                   prop_labelDatabase_ok
    , testProperty "Cannot label database with unbounded label"
                   prop_setDatabaseLabel_fail
    , testProperty "Cannot label collection-set with unbounded label"
                   prop_setCollectionSetLabel_fail
    ]
  , testGroup "Creating collections" [
      testProperty "Create ok with empty policies and bounded labels"
                   prop_createCollection_empty_ok 
    , testProperty "Create fail with empty policies and unbounded labels"
                   prop_createCollection_empty_fail
    ]
  , testGroup "Type check documents against policy" [
      testProperty "Type check ok when all fields exist and well-formed"
                   typeCheckDocument_all_named_exist 
    , testProperty "Type check fail when all fields don't exist and well-formed 1"
                   typeCheckDocument_all_named_exist_fail1
    , testProperty "Type check fail when all fields don't exist and well-formed 2"
                   typeCheckDocument_all_named_exist_fail2
  ]
  , testGroup "Policy application" [
      testProperty "Simple, all-public policy"
                   test_applyCollectionPolicyP_allPub 
    , testProperty "Simple, all-public policy. Field is pre-labeled to bottom"
                   test_applyCollectionPolicyP_allPub_field_bottom_fail
    , testProperty "Document label is above the collection clearance"
                   test_applyCollectionPolicyP_allPub_bad_doc_policy_fail
    , testProperty "Field label is above the collection clearance"
                   test_applyCollectionPolicyP_allPub_bad_field_policy_fail
    , testProperty "Simple data-depedent policy" 
                   test_applyCollectionPolicyP_label_by_field
  ]
  , testGroup "Insert" [
      testProperty "Simple, all-public policy insert"
                   test_basic_insert 
    , testProperty "Simple, insert with policy on document and field"
                   test_basic_insert_with_pl 
    , testProperty "Test insert after taint: failure"
                   test_basic_insert_fail 
  ]
  , testGroup "Find" [
      testProperty "Simple, all-public find"
                   test_basic_find
    , testProperty "Simple, find with policy on document and field"
                   test_basic_find_with_pl 
  ]
  , testGroup "Save" [
      testProperty "Simple, all-public save"
                   test_basic_save
    , testProperty "Simple, save with policy on document and field"
                   test_basic_save_with_pl 
    ]
  , testGroup "Labeled insert" [
      testProperty "Simple, all-public policy insert of already-labeled document"
                   test_basic_labeled_insert 
    , testProperty "Simple, all-public policy insert of already-labeled document fail"
                   test_basic_labeled_insert_fail 
    , testProperty "Simple, insert with policy on already-labeled document and field"
                   test_basic_labeled_insert_with_pl 
    , testProperty "Simple, fail insert with policy on already-labeled document and field"
                   test_basic_labeled_insert_with_pl_fail
    ]
    , testGroup "Labeled save" [
      testProperty "Simple, all-public policy save of already-labeled document"
                   test_basic_labeled_save 
    , testProperty "Simple, all-public policy save of already-labeled document fail"
                   test_basic_labeled_save_fail 
    , testProperty "Simple, save with policy on already-labeled document and field"
                   test_basic_labeled_save_with_pl
    ]
  ]

--
-- Pretend app running on behalf of some user
--

-- | Default clearance
defClr :: DCLabel
defClr = dcLabel (toComponent ("A" :: String)) dcTrue

-- | Initial clearance is set to some user's principal
doEvalDC :: DC a -> IO a
doEvalDC act = evalLIO act $
  LIOState { lioLabel = dcPub, lioClearance = defClr }

--
-- Define test policy module
--

-- | Empty registered policy module
newtype TestPM1 = TestPM1TCB DCPriv deriving (Show, Typeable)

instance PolicyModule TestPM1 where
  initPolicyModule p = return (TestPM1TCB p)

withTestPM1 :: (TestPM1 -> DBAction a) -> DC a
withTestPM1 = withPolicyModule

-- | Policy module that is not registered
data TestPM1Fake = TestPM1FakeTCB deriving Typeable

instance PolicyModule TestPM1Fake where
  initPolicyModule _ = return TestPM1FakeTCB

withTestPM1Fake :: (TestPM1Fake -> DBAction a) -> DC a
withTestPM1Fake = withPolicyModule

--
-- Create policy module configuration file
--

-- | DB config file path
dbConfFile :: FilePath
dbConfFile = "/tmp/test_hails_database.conf"

testPM1Principal :: String
testPM1Principal = "_testPM1"

-- | TestPM1's privileges
testPM1Priv :: DCPriv
testPM1Priv = mintTCB . dcPrivDesc $ testPM1Principal

-- | Only register TestPM1
mkDBConfFile :: IO ()
mkDBConfFile = do
  writeFile dbConfFile (unlines [show tpm1, show tpm2])
  setEnv "DATABASE_CONFIG_FILE" dbConfFile False
   where tpm1,tpm2 :: (String, String, String)
         tpm1 = (mkName (TestPM1TCB undefined), testPM1Principal, "testPM1_db")
         tpm2 = (mkName (TestPM2TCB undefined), testPM2Principal, "testPM2_db")

mkName :: PolicyModule pm => pm -> TypeName
mkName x = tyConPackage tp ++ ":" ++ tyConModule tp ++ "." ++ tyConName tp
  where tp = typeRepTyCon $ typeOf x

--
-- withPolicy tests
--

-- | Test that the loading of the TestPM1 policy module does not throw an
-- exception
test_withPolicyModuleP_ok :: Assertion
test_withPolicyModuleP_ok = do
  mkDBConfFile
  doEvalDC . withTestPM1 . const $  return ()

-- | Test that the loading of the TestPM1Fake policy module throws an
-- exception
test_withPolicyModuleP_fail :: Assertion
test_withPolicyModuleP_fail = do
  mkDBConfFile
  r <- paranoidDC $ withTestPM1Fake $ \_ ->  return ()
  case r of
    Left _ -> return ()
    Right _ -> assertFailure "withPolicyModule should fail with non-existant DB"
  

--
-- Testing label database
--

monadicDC :: PropertyM DC a -> Property
monadicDC (MkPropertyM m) =
 property $ unsafePerformIO `liftM` doEvalDC `liftM` m f
  where f = const . return . return .  property $ True 

-- | As if done in 'initPolicyModule' without bracket
initTestPM1 :: PMAction a -> DC a
initTestPM1 act = do
  ioTCB mkDBConfFile
  withTestPM1 . const . unPMActionTCB $ 
      withClearanceP' testPM1Priv $ act
  where withClearanceP' priv io = do
          c <- getClearance
          let lpriv = dcLabel (privDesc priv) (privDesc priv) `lub` c
          setClearanceP priv lpriv
          res <- io
          c' <- getClearance 
          setClearanceP priv (partDowngradeP priv c' c)
          return res

-- | Execute a monadic quickcheck action against policy module TestPM1
monadicPM1 :: (DCPriv -> PropertyM PMAction a) -> Property
monadicPM1 g =
 let (MkPropertyM m) = g testPM1Priv 
 in property $ unsafePerformIO `liftM` doEvalDC
                               `liftM` initTestPM1
                               `liftM` m f
  where f = const . return . return .  property $ True

-- | Execute a monadic quickcheck action against policy module TestPM1
monadicPM1_fail :: (DCPriv -> PropertyM PMAction a) -> Property
monadicPM1_fail g =
 let (MkPropertyM m) = g testPM1Priv 
 in property $ unsafePerformIO `liftM` doEvalDC
                               `liftM` initTestPM1'
                               `liftM` m f
  where f = const . return . return .  property $ True
        initTestPM1' act = (initTestPM1 act)
                            `catchTCB` (\_ -> return (property True))

--
-- Label database and collection-set
--

-- | Can label database with label bounded by current label and clearance
prop_labelDatabase_ok :: Property
prop_labelDatabase_ok = monadicPM1 $ \priv ->
  forAllM arbitrary $ \ldb -> 
  forAllM arbitrary $ \lcol -> do
    l <- run $ liftDB getLabel
    c <- run $ liftDB getClearance
    pre $ canFlowToP priv l ldb  && ldb `canFlowTo` c
    pre $ canFlowToP priv l lcol  && lcol `canFlowTo` c
    run $ labelDatabaseP priv ldb lcol
    Q.assert True

-- | Cannot label database with label outside current label/clearance
prop_setDatabaseLabel_fail :: Property
prop_setDatabaseLabel_fail = monadicPM1_fail $ \priv -> do 
  forAllM arbitrary $ \ldb -> do
    l <- run $ liftDB getLabel
    c <- run $ liftDB getClearance
    pre . not $ canFlowToP priv l ldb  && ldb `canFlowTo` c
    run $ setDatabaseLabelP priv ldb
    Q.assert False

-- | Cannot label colelction-set with label outside current label/clearance
prop_setCollectionSetLabel_fail :: Property
prop_setCollectionSetLabel_fail = monadicPM1_fail $ \priv -> do 
  forAllM arbitrary $ \lcol -> do
    l <- run $ liftDB getLabel
    c <- run $ liftDB getClearance
    pre . not $ canFlowToP priv l lcol  && lcol `canFlowTo` c
    run $ setCollectionSetLabelP priv lcol
    Q.assert False


--
-- Create collections
--

prop_createCollection_empty_ok :: Property
prop_createCollection_empty_ok = monadicPM1 $ \priv ->
  forAllM arbitrary $ \lcol -> 
  forAllM arbitrary $ \ccol -> do
    l <- run $ liftDB getLabel
    c <- run $ liftDB getClearance
    pre $ canFlowToP priv l lcol  && lcol `canFlowTo` c
    pre $ canFlowToP priv l ccol  && ccol `canFlowTo` c
    let policy = CollectionPolicy {
                    documentLabelPolicy = const dcPub
                  , fieldLabelPolicies = Map.empty }
    run $ createCollectionP priv "somefuncollection" lcol ccol policy
    Q.assert True

prop_createCollection_empty_fail :: Property
prop_createCollection_empty_fail = monadicPM1_fail $ \priv ->
  forAllM arbitrary $ \lcol -> 
  forAllM arbitrary $ \ccol -> do
    l <- run $ liftDB getLabel
    c <- run $ liftDB getClearance
    pre . not $ (canFlowToP priv l lcol  && lcol `canFlowTo` c) && 
                (canFlowToP priv l ccol  && ccol `canFlowTo` c)
    let policy = CollectionPolicy {
                    documentLabelPolicy = const dcPub
                  , fieldLabelPolicies = Map.empty }
    run $ createCollectionP priv "somefuncollection" lcol ccol policy
    Q.assert False


--
-- Type check doc
--

-- | Remove any policy labeled values
removePolicyLabeled :: HsonDocument -> HsonDocument
removePolicyLabeled = filter (not . isPolicyLabeled)

labelOfPL :: PolicyLabeled -> DCLabel
labelOfPL (HasPolicyTCB lv) = labelOf lv
labelOfPL _ = error "should have been labeled"

isPolicyLabeled :: HsonField -> Bool
isPolicyLabeled (HsonField _ (HsonLabeled _)) = True
isPolicyLabeled  _ = False

-- | Policies used by the typeCheckDocument tests
typeCheckDoc_policies :: Map FieldName FieldPolicy
typeCheckDoc_policies =
     Map.fromList [ ("s1", SearchableField)
                  , ("s2", SearchableField)
                  , ("p1", FieldPolicy (const dcPub))
                  , ("p2", FieldPolicy (const dcPub)) ]

-- | Check that all fields of 'typeCheckDoc_policies' exist in a
-- document and are typed-correctly.
typeCheckDocument_all_named_exist :: Property
typeCheckDocument_all_named_exist = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- hasPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2] 
            `merge` doc2
  run $ typeCheckDocument typeCheckDoc_policies doc
  Q.assert True

-- | Check that all fields of 'typeCheckDoc_policies' exist in a
-- document and are typed-correctly: fail
-- A searchable/policy labeled field does not exist
typeCheckDocument_all_named_exist_fail1 :: Property
typeCheckDocument_all_named_exist_fail1 = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- hasPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  rm <- pick $ elements ["s1", "s2", "p1", "p2" ]
  let doc = exclude [rm] $
            [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2] 
            `merge` doc2
  res <- run $ (typeCheckDocument typeCheckDoc_policies doc >> return False)
                  `catchTCB` (const $ return True)
  Q.assert res

-- | Check that all fields of 'typeCheckDoc_policies' exist in a
-- document and are typed-correctly: fail
-- A policy labeled field not named by the policy exists
typeCheckDocument_all_named_exist_fail2 :: Property
typeCheckDocument_all_named_exist_fail2 = monadicDC $ do
  doc2 <- pick arbitrary
  pre $ any isPolicyLabeled doc2
  pl1 <- hasPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  res <- run $ (typeCheckDocument typeCheckDoc_policies doc >> return False)
                  `catchTCB` (const $ return True)
  Q.assert res

--
-- Test applyCollection policy
--

-- | Apply all-public policies
test_applyCollectionPolicyP_allPub :: Property
test_applyCollectionPolicyP_allPub = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- needPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  ldoc <- run $ applyCollectionPolicyP noPriv col doc
  Q.assert $ labelOf ldoc == dcPub
  let doc' = unlabelTCB ldoc
  Q.assert $ labelOfPL (at "p1" doc') == dcPub
  Q.assert $ labelOfPL (at "p2" doc') == dcPub
  Q.assert . not $ any isPolicyLabeled $  exclude ["p1", "p2"] doc' 
    where col = collectionTCB "myColl" dcPub dcPub cPolicy
          cPolicy = CollectionPolicy {
              documentLabelPolicy = const dcPub 
            , fieldLabelPolicies  = typeCheckDoc_policies }

-- | Apply all-public policies, field has higher integrity: fail
test_applyCollectionPolicyP_allPub_field_bottom_fail :: Property
test_applyCollectionPolicyP_allPub_field_bottom_fail = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- needPolicy `liftM` pick arbitrary
  pl2 <- hasPolicy `liftM` pick arbitrary
  pre $ labelOfPL pl2 /= dcPub
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  res <- run $ (applyCollectionPolicyP noPriv col doc >> return False)
                `catchTCB` (const $ return True)
  Q.assert res
    where col = collectionTCB "myColl" dcPub dcPub cPolicy
          cPolicy = CollectionPolicy {
              documentLabelPolicy = const dcPub 
            , fieldLabelPolicies  = typeCheckDoc_policies }

-- | Apply all-public policies, document policy is above clearance
test_applyCollectionPolicyP_allPub_bad_doc_policy_fail :: Property
test_applyCollectionPolicyP_allPub_bad_doc_policy_fail = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- needPolicy `liftM` pick arbitrary
  pl2 <- hasPolicy `liftM` pick arbitrary
  pre $ labelOfPL pl2 /= dcPub
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  res <- run $ (applyCollectionPolicyP noPriv col doc >> return False)
                `catchTCB` (const $ return True)
  Q.assert res
    where col = collectionTCB "myColl" dcPub dcPub cPolicy
          cPolicy = CollectionPolicy {
              documentLabelPolicy = const defClr
            , fieldLabelPolicies  = typeCheckDoc_policies }

-- | Apply all-public policies, document policy is above clearance
test_applyCollectionPolicyP_allPub_bad_field_policy_fail :: Property
test_applyCollectionPolicyP_allPub_bad_field_policy_fail = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- needPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  res <- run $ (applyCollectionPolicyP noPriv col doc >> return False)
                `catchTCB` (const $ return True)
  Q.assert res
    where col = collectionTCB "myColl" dcPub dcPub cPolicy
          cPolicy = CollectionPolicy {
              documentLabelPolicy = const dcPub
            , fieldLabelPolicies  = Map.fromList [ ("s1", SearchableField)
                                  , ("s2", SearchableField)
                                  , ("p1", FieldPolicy (const dcPub))
                                  , ("p2", FieldPolicy (const defClr)) ] }

-- | Apply all-public policies
test_applyCollectionPolicyP_label_by_field :: Property
test_applyCollectionPolicyP_label_by_field = monadicDC $ do
  doc2 <- removePolicyLabeled `liftM` pick arbitrary
  pl1 <- needPolicy `liftM` pick arbitrary
  pl2 <- needPolicy `liftM` pick arbitrary
  let doc = [ "s1" -: prin, "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int), "p1" -: pl1, "p2" -: pl2]
            `merge` doc2
  ldoc <- run $ applyCollectionPolicyP priv col doc
  Q.assert $ labelOf ldoc == lbl
  let doc' = unlabelTCB ldoc
  Q.assert $ labelOfPL (at "p1" doc') == lbl
  Q.assert $ labelOfPL (at "p2" doc') == lbl
  Q.assert . not $ any isPolicyLabeled $  exclude ["p1", "p2"] doc' 
  Q.assert True
    where col = collectionTCB "myColl" dcPub lbl cPolicy
          fpol d  =  let n = at "s1" d :: String
                     in dcLabel (n \/ ("A" :: String)) dcTrue
          lbl     = dcLabel (prin \/ ("A" :: String)) dcTrue
          prin    = "w00t" :: String
          priv    = mintTCB . dcPrivDesc $ prin
          cPolicy = CollectionPolicy {
              documentLabelPolicy = fpol
            , fieldLabelPolicies  = Map.fromList [ ("s1", SearchableField)
                                      , ("s2", SearchableField)
                                      , ("p1", FieldPolicy fpol)
                                      , ("p2", FieldPolicy fpol)] }
--
-- Test insert and find
--

-- | Execute a mongo action against the testPM2 database
withMongo :: Mongo.Action IO a -> IO a
withMongo act = do
  pipe <- Mongo.runIOE $ Mongo.connect (Mongo.host "localhost")
  res <- Mongo.access pipe Mongo.master "testPM2_db" act
  Mongo.close pipe
  case res of
    Left f -> throwIO (userError $ "Failed with " ++ show f)
    Right v -> return v


testPM2Principal :: String
testPM2Principal = "_testPM2"

-- | TestPM2's privileges
testPM2Priv :: DCPriv
testPM2Priv = mintTCB . dcPrivDesc $ testPM2Principal

-- | Empty registered policy module
newtype TestPM2 = TestPM2TCB DCPriv deriving (Show, Typeable)

instance PolicyModule TestPM2 where
  initPolicyModule p = do
    -- label db & collection-set
    labelDatabaseP p dcPub lDB
    -- create public storage
    createCollectionP p "public" dcPub dcPub cPubPolicy
    -- create collection with a policy-label for document and field
    createCollectionP p "simple_pl" dcPub cCol cSimplePlPolicy
    return $ TestPM2TCB p
        where this = privDesc p
              cCol = dcLabel this dcTrue
              lDB = dcLabel dcTrue (privDesc p)
              cPubPolicy = CollectionPolicy { documentLabelPolicy = const dcPub
                                            , fieldLabelPolicies = Map.empty }
              fpol d  =  let n = at "s" d :: String
                         in dcLabel (n \/ this) dcTrue
              cSimplePlPolicy = CollectionPolicy {
                  documentLabelPolicy = fpol
                , fieldLabelPolicies = Map.fromList [ ("pl", FieldPolicy fpol)
                                                    , ("s", SearchableField)] }

withTestPM2 :: (TestPM2 -> DBAction a) -> DC a
withTestPM2 f = do
  ioTCB mkDBConfFile
  --ioTCB $ withMongo $ Mongo.delete (Mongo.select [] "public")
  --ioTCB $ withMongo $ Mongo.delete (Mongo.select [] "simple_pl")
  withPolicyModule f 

-- | Test insert in all-public collection
test_basic_insert :: Property
test_basic_insert = monadicDC $ do
  doc <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  _id <- run $ withTestPM2 $ const $ do
                         insert "public" doc
  mdoc <- run $ ioTCB $ withMongo $ Mongo.findOne
                              (Mongo.select ["_id" Mongo.=: _id] "public")
  let bdoc = fromJust mdoc
      doc' = sortDoc $ merge ["_id" -: _id] doc
  Q.assert $ isJust mdoc &&
            (sortDoc (dataBsonDocToHsonDocTCB bdoc) == doc')

-- | Test insert containing a policy labeled value
test_basic_insert_with_pl :: Property
test_basic_insert_with_pl = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  pl   <- needPolicy `liftM` pick arbitrary
  let s = "A" :: String
  let doc = merge ["s" -: s, "pl" -: pl] doc0
  _id <- run $ withTestPM2 $ const $ do
                         insert "simple_pl" doc
  mdoc <- run $ ioTCB $ withMongo $ Mongo.findOne
                              (Mongo.select ["_id" Mongo.=: _id] "simple_pl")
  let bdoc = fromJust mdoc
      doc' = sortDoc $ merge ["_id" -: _id] doc
  Q.assert $ isJust mdoc &&
            (sortDoc (dataBsonDocToHsonDocTCB bdoc) == doc')

-- | Test insert after taint: failure.
test_basic_insert_fail :: Property
test_basic_insert_fail = monadicDC $ do
  res <- run $ (withTestPM2 $ const $ do
    getClearance >>= taint
    insert_ "public" (["my" -: (1::Int)] :: HsonDocument)
    return False) `catchLIO` (\(_::SomeException) -> return True)
  Q.assert res


-- | Test insert in all-public collection
test_basic_find :: Property
test_basic_find = monadicDC $ do
  let doc = [ "s1" -: (1 :: Int), "s3" -: (3 :: Int), "s2" -: (2 :: Int)
            , "x1" -: (4 :: Int)]
  _id <- run $ withTestPM2 $ const $ insert "public" doc
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "public")
  Q.assert $ isJust mdoc
  doc' <- run $ unlabel $ fromJust mdoc
  Q.assert $ sortDoc doc' == sortDoc (merge ["_id" -: _id] doc)

-- | Test find containing a policy labeled value
test_basic_find_with_pl :: Property
test_basic_find_with_pl = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  plv  <- pick arbitrary
  let pl  = needPolicy (plv :: BsonValue)
      s = "A" :: String
      doc = merge ["s" -: s , "pl" -: pl] doc0
  _id <- run $ withTestPM2 $ const $ insert "simple_pl" doc
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "simple_pl")
  Q.assert $ isJust mdoc
  let priv = mintTCB . dcPrivDesc $ s
  doc' <- run $ unlabelP priv $ fromJust mdoc
  Q.assert $ (sortDoc . exclude ["pl"] $ doc') ==
             (sortDoc . merge ["_id" -: _id] . exclude ["pl"] $ doc)
  let ~mlv@(Just lv) =  getPolicyLabeled $ "pl" `at` doc'
  Q.assert $ isJust mlv && unlabelTCB lv == plv

-- | Test save in all-public collection
test_basic_save :: Property
test_basic_save = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  doc1 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  _id <- run $ withTestPM2 $ const $ insert "public" doc0
  let doc = merge ["_id" -: _id] doc1
  run $ withTestPM2 $ const $ save "public" doc
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "public")
  Q.assert $ isJust mdoc
  doc' <- run $ unlabel $ fromJust mdoc
  Q.assert $ sortDoc doc == sortDoc doc'

-- | Test save containing a policy labeled value
test_basic_save_with_pl :: Property
test_basic_save_with_pl = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  plv  <- pick arbitrary
  let pl  = needPolicy (plv :: BsonValue)
  let s = "A" :: String
      priv = mintTCB . dcPrivDesc $ s
      doc1 = merge ["s" -: s , "pl" -: pl] doc0
  _id <- run $ withTestPM2 $ const $ insert "simple_pl" doc1
  let doc2 = merge ["_id" -: _id, "x" -: ("f00ba12" :: String)] doc1
  run $ withTestPM2 $ const $ saveP priv "simple_pl" $ doc2
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "simple_pl")
  Q.assert $ isJust mdoc
  doc' <- run $ unlabelP priv $ fromJust mdoc
  Q.assert $ (sortDoc . exclude ["pl"] $ doc') ==
             (sortDoc . exclude ["pl"] $ doc2)
  let ~mlv@(Just lv) =  getPolicyLabeled $ "pl" `at` doc'
  Q.assert $ isJust mlv && unlabelTCB lv == plv

-- | Test labeled insert in all-public collection
test_basic_labeled_insert :: Property
test_basic_labeled_insert = monadicDC $ do
  doc <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  ldoc <- run $ label dcPub doc
  _id <- run $ withTestPM2 $ const $ do
                         insert "public" ldoc
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "public")
  Q.assert $ isJust mdoc && labelOf (fromJust mdoc) == dcPub
  doc' <- run $ unlabel $ fromJust mdoc
  Q.assert $ sortDoc doc' == sortDoc (merge ["_id" -: _id] doc)

-- | Test labled insert containing a policy labeled value
test_basic_labeled_insert_with_pl :: Property
test_basic_labeled_insert_with_pl = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  let s = "A" :: String
      l = dcLabel (s \/ testPM2Principal) dcTrue
  plv <- pick arbitrary
  pl  <- run $ label l (plv :: BsonValue)
  let doc = merge ["s" -: s, "pl" -: pl] doc0
  ldoc <- run $ label l doc
  _id <- run $ withTestPM2 $ const $ insert "simple_pl" ldoc
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "simple_pl")
  Q.assert $ isJust mdoc
  let doc' = unlabelTCB $ fromJust mdoc
      doc'' = merge ["_id" -: _id] doc
  Q.assert $ (sortDoc . exclude ["pl"] $ doc') ==
             (sortDoc . exclude ["pl"] $ doc'')
  let ~mlv@(Just lv) =  getPolicyLabeled $ "pl" `at` doc'
  Q.assert $ isJust mlv && unlabelTCB lv == plv

-- | Test labeled insert in all-public collection
test_basic_labeled_insert_fail :: Property
test_basic_labeled_insert_fail = monadicDC $ do
  doc <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  clr <- run $ getClearance
  ldoc <- run $ label clr doc
  res <- run $ (withTestPM2 $ const $ do
                         insert_ "public" ldoc
                         return False) `catchLIO` (\(_::SomeException) -> return True)
  Q.assert res

-- | Test labled insert containing a policy labeled value, fail
test_basic_labeled_insert_with_pl_fail :: Property
test_basic_labeled_insert_with_pl_fail = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  let s = "A" :: String
      lOfdoc = dcLabel (s \/ testPM2Principal) dcTrue
      l = dcLabel (s \/ testPM2Principal \/ ("failureCause" :: String)) dcTrue
  plv <- pick arbitrary
  pl  <- run $ label l (plv :: BsonValue)
  let doc = merge ["s" -: s, "pl" -: pl] doc0
  ldoc <- run $ label lOfdoc doc
  res <- run $ (withTestPM2 $ const $ do
                         insert_ "simple_pl" ldoc
                         return False) `catchLIO` (\(_::SomeException) -> return True)
  Q.assert res

-- | Test labeled save in all-public collection
test_basic_labeled_save :: Property
test_basic_labeled_save = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  doc1 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  ldoc0 <- run $ label dcPub doc0
  _id <- run $ withTestPM2 $ const $ insert "public" ldoc0
  ldoc1 <- run $ label dcPub $ merge ["_id" -: _id] doc1
  run $ withTestPM2 $ const $ save "public" ldoc1
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "public")
  Q.assert $ isJust mdoc && labelOf (fromJust mdoc) == dcPub
  doc' <- run $ unlabel $ fromJust mdoc
  Q.assert $ sortDoc doc' == sortDoc (unlabelTCB ldoc1)

-- | Test labled save containing a policy labeled value
test_basic_labeled_save_with_pl :: Property
test_basic_labeled_save_with_pl = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  doc1 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  let s = "A" :: String
      lOfdoc = dcLabel (s \/ testPM2Principal \/ ("B" :: String)) dcTrue
      l = dcLabel (s \/ testPM2Principal) dcTrue
  plv <- pick arbitrary
  pl  <- run $ label l (plv :: BsonValue)
  ldoc0 <- run $ label lOfdoc $ merge ["s" -: s, "pl" -: pl] doc0
  _id <- run $ withTestPM2 $ const $ insert "simple_pl" ldoc0
  ldoc1 <- run $ label dcPub $ merge ["_id" -: _id,"s" -: s, "pl" -: pl] doc1
  run $ withTestPM2 $ const $ save "simple_pl" ldoc1
  mdoc <- run $ withTestPM2 $ const $ findOne (select ["_id" -: _id] "simple_pl")
  Q.assert $ isJust mdoc
  let doc' = unlabelTCB $ fromJust mdoc
      doc'' = merge ["_id" -: _id] $ unlabelTCB ldoc1
  Q.assert $ (sortDoc . exclude ["pl"] $ doc') ==
             (sortDoc . exclude ["pl"] $ doc'')
  let ~mlv@(Just lv) =  getPolicyLabeled $ "pl" `at` doc'
  Q.assert $ isJust mlv && unlabelTCB lv == plv

-- | Test labeled save in all-public collection
test_basic_labeled_save_fail :: Property
test_basic_labeled_save_fail = monadicDC $ do
  doc0 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  doc1 <- (removePolicyLabeled . clean) `liftM` pick arbitrary
  ldoc0 <- run $ label dcPub doc0
  _id <- run $ withTestPM2 $ const $ insert "public" ldoc0
  clr <- run $ getClearance
  ldoc1 <- run $ label clr $ merge ["_id" -: _id] doc1
  res <- run $ (withTestPM2 $ const $ do
                  save "public" ldoc1
                  return False) `catchLIO` (\(_::SomeException) -> return True)
  Q.assert res
