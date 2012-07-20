{-# LANGUAGE OverloadedStrings #-}
module AuthTests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Hails.HttpServer.Auth
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test

authTests :: Test
authTests = testGroup "Auth"
  [ testCase "Require Login on X-Hails-Login header" $ runSession (do
        resp <- request undefined
        assertHeader "TestHeader" "MyHeaderVal" resp) $
      requireLoginMiddleware (responseLBS status301 [("TestHeader", "MyHeaderVal")] "") $
      const . return $ responseLBS status401 [("x-hails-login", "yes")] ""
  , testCase "No login if not X-Hails-Login header" $ runSession (do
        resp <- request undefined
        assertNoHeader "TestHeader" resp) $
      requireLoginMiddleware (responseLBS status301 [("TestHeader", "ShouldNotBeThere")] "") $
      const . return $ responseLBS status200 [] ""
  ]

