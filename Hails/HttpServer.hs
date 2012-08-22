{-# LANGUAGE OverloadedStrings #-}
{- |

This module exports methods for constructing a WAI-based HTTP server to serve
Hails 'Application's.

-}
module Hails.HttpServer
  ( devHailsHandler
  , hailsApplication
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import qualified Network.Wai as W

import Hails.HttpServer.Auth
import Hails.HttpServer.Types
import LIO
import LIO.TCB
import LIO.DCLabel
import LIO.DCLabel.Privs.TCB
import LIO.Labeled.TCB

-- | Hails 'Middleware' that ensures the 'Response' from the application is
-- readable by the client's browser (as determined by the result label of the
-- app computation and the label of the browser). If the response is not
-- readable by the browser, the middleware sends a 403 (unauthorized) response
-- instead.
browserGuardMiddleware :: Middleware
browserGuardMiddleware hailsApp conf req = do
  putLIOStateTCB $ LIOState { lioLabel = dcPub, lioClearance = browserLabel conf}
  response <- hailsApp conf req
  resultLabel <- getLabel
  return $
    if resultLabel `canFlowTo` (browserLabel conf)
      then response
      else Response status403 [] ""

-- Remove anything from the response that could cause inadvertant\
-- declasification (e.g. Cookies)
sanitizeResp :: Middleware
sanitizeResp hailsApp conf req = do
  response <- hailsApp conf req
  return $ removeResponseHeader response hCookie
  

-- | Dumly run a Hails 'Application' and return a corresponding WAI
-- 'W.Applicaiton'. Should generally not be run alone by combined with
-- middleware (e.g. 'browserGuardMiddleware') that ensures the safety of the
-- computation and response.
transformHailsApp :: Application -> W.Application
transformHailsApp hailsApp req0 = do
  hailsRequest <- waiToHailsReq req0
  let conf = getRequestConf hailsRequest
  response <- liftIO $ evalDC $ do
    let lreq = labelTCB (requestLabel conf) hailsRequest
    hailsApp conf lreq
  return $ hailsToWaiResponse response

-- | Adds the header \"X-Hails-Sensitive: Yes\" to the response if the label of
-- the computation is above 'lpub'.
addXHailsSensitive :: Middleware
addXHailsSensitive happ p req = do
  response <- happ p req
  resultLabel <- getLabel
  if resultLabel `canFlowTo` dcPub
    then return response
    else return $ addResponseHeader response ("X-Hails-Sensitive", "Yes")

-- | Returns a secure Hails app such that the result 'Response' is guaranteed
-- to be safe to transmit to the client's browser.
secureApplication :: Middleware
secureApplication = addXHailsSensitive . browserGuardMiddleware

-- | Safely wraps a Hails 'Application' in a WAI 'W.Application' that can be
-- run by an application server.
hailsApplication :: Application -> W.Application
hailsApplication = transformHailsApp . secureApplication

-- | A default Hails handler for development environments. Safely runs a Hails
-- 'Application', using basic HTTP authentication for authenticating users.
-- However, authentication will accept any username/password pair.
devHailsHandler :: Application -> W.Application
devHailsHandler = devBasicAuth "Hails" . hailsApplication

--
-- Helper
--

-- | Get the browser label (secrecy of the user), request label (integrity of
-- the user), and application privilege (minted with the app's cannonical name)
getRequestConf :: Request -> RequestConfig
getRequestConf req =
  let headers = requestHeaders req
      userName  = toComponent `fmap` lookup "x-hails-user" headers
      appName  = S8.unpack . S8.takeWhile (/= '.') $ serverName req
      appPriv = DCPrivTCB $ toComponent appName
  in RequestConfig
      { browserLabel = maybe dcPub (\un -> dcLabel un anybody) userName
      , requestLabel = maybe dcPub (\un -> dcLabel anybody un) userName
      , appPrivilege = appPriv }

