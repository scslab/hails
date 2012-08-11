{-# LANGUAGE OverloadedStrings,
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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text ()

import Control.Monad
import Control.Monad.Base

import Data.Typeable

import LIO
import LIO.Labeled.TCB
import LIO.TCB (ioTCB, catchTCB)
import LIO.Privs.TCB (mintTCB)
import LIO.DCLabel
import LIO.DCLabel.Instances ()

import Hails.Data.Hson
import Hails.Data.Hson.TCB
import Hails.Data.Hson.Instances ()
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
  writeFile dbConfFile (unlines [show tpm1])
  setEnv "DATABASE_CONFIG_FILE" dbConfFile False
   where tpm1 :: (String, String, String)
         tpm1 = (mkName (TestPM1TCB undefined), testPM1Principal, "testPM1_db")

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
  doEvalDC $ withTestPM1 $ \_ ->  return ()

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

-- | As if done in 'initPolicyModule'
initTestPM1 :: PMAction a -> DC a
initTestPM1 act = do
  ioTCB mkDBConfFile
  withTestPM1 $ \(TestPM1TCB priv) -> do
    c <- getClearance
    let lpriv = dcLabel (privDesc priv) (privDesc priv) `lub` c
    bracketP priv
             -- Raise clearance:
             (setClearanceP priv lpriv)
             -- Lower clearance:
             (const $ do c' <- getClearance 
                         setClearanceP priv (partDowngradeP priv c' c))
             -- Execute policy module entry point, in between:
             (const $ unPMActionTCB act)

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
    l <- run $ liftBase getLabel
    c <- run $ liftBase getClearance
    pre $ canFlowToP priv l ldb  && ldb `canFlowTo` c
    pre $ canFlowToP priv l lcol  && lcol `canFlowTo` c
    run $ labelDatabaseP priv ldb lcol
    Q.assert True

-- | Cannot label database with label outside current label/clearance
prop_setDatabaseLabel_fail :: Property
prop_setDatabaseLabel_fail = monadicPM1_fail $ \priv -> do 
  forAllM arbitrary $ \ldb -> do
    l <- run $ liftBase getLabel
    c <- run $ liftBase getClearance
    pre . not $ canFlowToP priv l ldb  && ldb `canFlowTo` c
    run $ setDatabaseLabelP priv ldb
    Q.assert False

-- | Cannot label colelction-set with label outside current label/clearance
prop_setCollectionSetLabel_fail :: Property
prop_setCollectionSetLabel_fail = monadicPM1_fail $ \priv -> do 
  forAllM arbitrary $ \lcol -> do
    l <- run $ liftBase getLabel
    c <- run $ liftBase getClearance
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
    l <- run $ liftBase getLabel
    c <- run $ liftBase getClearance
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
    l <- run $ liftBase getLabel
    c <- run $ liftBase getClearance
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
