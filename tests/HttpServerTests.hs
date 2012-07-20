{-# LANGUAGE OverloadedStrings #-}
module HttpServerTests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Hails.HttpServer
import Hails.Types
import Network.HTTP.Types
import qualified Network.Wai as W
import Network.Wai.Test

clearanceViolatingApp :: Application

httpServerTests :: Test
httpServerTests = testGroup "hailsApplication"
  [ testCase "Restricts current label" $ runSession (do
        resp <- request undefined
        assertHeader "TestHeader" "MyHeaderVal" resp) $
      requireLoginMiddleware (responseLBS status301 [("TestHeader", "MyHeaderVal")] "") $
      const . return $ responseLBS status401 [("x-hails-login", "yes")] ""
  ]

