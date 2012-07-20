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

hailsApplication :: Application -> W.Application
hailsApplication hailsApp req0 = do
  hailsRequest <- waiToHailsReq req0
  let (RequestConfig browserLabel requestLabel appPrivilege) = getRequestConf hailsRequest
  (response, resultLabel) <- liftIO . evalDC $ do
    lowerClr browserLabel
    setLabelTCB lpub
    let lreq = labelTCB requestLabel hailsRequest
    addXHailsSensitive hailsApp appPrivilege lreq
  return $
    if resultLabel `leq` browserLabel then hailsToWaiResponse response
      else W.responseLBS status403 [] ""

addXHailsSensitive :: Middleware
addXHailsSensitive happ p req = do
  response <- happ p req
  resultLabel <- getLabel
  if resultLabel `leq` lpub
    then return response
    else return $ addResponseHeader response ("X-Hails-Sensitive", "Yes")

data RequestConfig = RequestConfig { browserLabel :: DCLabel
                                   , requestLabel :: DCLabel
                                   , appPrivilege :: DCPrivTCB }

--
-- Helper
--

-- | Get the browser label, application name, and new (safe)
-- request. Note: all cookies are removed.
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

