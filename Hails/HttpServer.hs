{-# LANGUAGE OverloadedStrings #-}
module Hails.HttpServer (hailsApplication) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import qualified Network.Wai as W

import Hails.HttpServer.Types
import DCLabel.TCB
import LIO.TCB
import LIO.DCLabel

browserGuardMiddleware :: Middleware
browserGuardMiddleware hailsApp conf req = do
  lowerClr $ browserLabel conf
  setLabelTCB lpub
  response <- hailsApp conf req
  resultLabel <- getLabel
  return $
    if resultLabel `canflowto` (browserLabel conf)
      then response
      else Response status403 [] ""

transformHailsApp :: Application -> W.Application
transformHailsApp hailsApp req0 = do
  hailsRequest <- waiToHailsReq req0
  let conf = getRequestConf hailsRequest
  (response, _) <- liftIO . evalDC $ do
    let lreq = labelTCB (requestLabel conf) hailsRequest
    hailsApp conf lreq
  return $ hailsToWaiResponse response

addXHailsSensitive :: Middleware
addXHailsSensitive happ p req = do
  response <- happ p req
  resultLabel <- getLabel
  if resultLabel `leq` lpub
    then return response
    else return $ addResponseHeader response ("X-Hails-Sensitive", "Yes")

secureApplication :: Middleware
secureApplication = addXHailsSensitive . browserGuardMiddleware

hailsApplication :: Application -> W.Application
hailsApplication = transformHailsApp . secureApplication

--
-- Helper
--

-- | Get the browser label (secrecy of the user), request label (integrity of
-- the user), and application privilege (minted with the app's cannonical name)
getRequestConf :: Request -> RequestConfig
getRequestConf req =
  let headers = requestHeaders req
      userName  = principal `fmap` lookup "x-hails-user" headers
      appName  = S8.unpack . S8.takeWhile (/= '.') $ serverName req
      appPriv = createPrivTCB $ newPriv appName
  in RequestConfig
      { browserLabel = maybe lpub (\un -> newDC un (<>)) userName
      , requestLabel = maybe lpub (\un -> newDC (<>) un) userName
      , appPrivilege = appPriv }

