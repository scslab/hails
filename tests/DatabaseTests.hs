{-# LANGUAGE DeriveDataTypeable #-}

module DatabaseTests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q

import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Base

import Data.Typeable

import LIO
import LIO.TCB (ioTCB, catchTCB)
import LIO.Privs.TCB (mintTCB)
import LIO.DCLabel
import LIO.DCLabel.Instances ()

import Hails.PolicyModule
import Hails.PolicyModule.TCB
import Hails.Database.Core

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
  ]

--
-- Pretend app running on behalf of some user
--

-- | Initial clearance is set to some user's principal
doEvalDC :: DC a -> IO a
doEvalDC act = evalLIO act $
  LIOState { lioLabel = dcPub
           , lioClearance = dcLabel (toComponent "A") dcTrue }

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
    run $ createCollectionP priv (T.pack "somefuncollection") lcol ccol policy
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
    run $ createCollectionP priv (T.pack "somefuncollection") lcol ccol policy
    Q.assert False
