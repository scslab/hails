{-# LANGUAGE OverloadedStrings #-}
module AuthTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Hails.HttpServer.Auth
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test

tests :: [Test]
tests = [authTest]

authTest :: Test
authTest = testGroup "Auth"
  [ testCase "Require Login on X-Hails-Login header" $ runSession (do
        resp <- request undefined
        assertHeader "TestHeader" "MyHeaderVal" resp) $
      requireLoginMiddleware (return $ responseLBS status301 [("TestHeader", "MyHeaderVal")] "") $
      const . return $ responseLBS status401 [("x-hails-login", "yes")] ""
  , testCase "No login if not X-Hails-Login header" $ runSession (do
        resp <- request undefined
        assertNoHeader "TestHeader" resp) $
      requireLoginMiddleware (return $ responseLBS status301 [("TestHeader", "ShouldNotBeThere")] "") $
      const . return $ responseLBS status200 [] ""
  ]

