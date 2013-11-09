{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{- |

This module exports the core of the Hails HTTP server. Specifically it
defines basic types, such as HTTP 'Request' and 'Response', used by
the Hails web server and untrusted Hails 'Application's. 

At a high level, a Hails 'Application', is a function from 'Request'
to 'Response' in the 'DC' monad. Every application response is
sanitized and sanity checked with the 'secureApplication'
'Middleware'. Moreover, every 'Request' is sanitized with 'sanitizeReq'
before handed over to authenticators.

Hails uses Wai, and as such we provide a function for converting
Hails 'Application's to Wai 'W.Applicatoin's: 'execHailsApplication'.

-}
module Hails.HttpServer (
  module Hails.HttpServer.Types
  -- ** Execute Hails application
  , execHailsApplication
  -- ** Middleware used by Hails
  , sanitizeReqMiddleware
  , browserLabelGuard
  , guardSensitiveResp 
  , sanitizeResp
  , catchAllExceptions
  -- * Network types
  , module Network.HTTP.Types
  ) where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Conduit.List hiding (head)
import           Data.Monoid

import           Control.Monad (liftM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Error.Class


import           Network.HTTP.Types
import           Network.URI (isURI)
import qualified Network.Wai as W
import qualified Network.Wai.Application.Static as W
import           Network.Wai.Middleware.MethodOverridePost

import           LIO
import           LIO.TCB
import           LIO.DCLabel

import           Hails.HttpServer.Types

import           System.IO
import           Data.Time (getCurrentTime)

import LIO.RCLabel
import LIO.RCRef
import LIO.SafeCopy
import Debug.Trace

-- | Convert a WAI 'W.Request' to a Hails 'Request' by consuming the
-- body into a 'L.ByteString'. The 'requestTime' is set to the
-- current time at the time this action is executed (which is when
-- the app is invoked).
waiToHailsReq :: W.Request -> ResourceT IO Request
waiToHailsReq req = do
  curTime <- liftIO getCurrentTime
  body <- fmap L.fromChunks $ W.requestBody req $$ consume
  return $ Request { requestMethod = W.requestMethod req
                   , httpVersion = W.httpVersion req
                   , rawPathInfo = W.rawPathInfo req
                   , rawQueryString = W.rawQueryString req
                   , serverName = W.serverName req
                   , serverPort = W.serverPort req
                   , requestHeaders = W.requestHeaders req
                   , isSecure = W.isSecure req
                   , remoteHost = W.remoteHost req
                   , pathInfo = W.pathInfo req
                   , queryString = W.queryString req
                   , requestBody = body 
                   , requestTime = curTime }

-- | Remove any unsafe headers, in this case only @X-Hails-User@.
sanitizeReqMiddleware :: W.Middleware
sanitizeReqMiddleware app req =  app $ req { W.requestHeaders = headers }
  where headers = List.filter ((/= "X-Hails-User") . fst) $ W.requestHeaders req

-- | Convert a Hails 'Response' to a WAI 'W.Response'
hailsToWaiResponse :: Response -> W.Response
hailsToWaiResponse (Response stat rhd body) = W.responseLBS stat rhd body

-- | Hails 'Middleware' that ensures the 'Response' from the
-- application is readable by the client's browser (as determined by the
-- result label of the app computation and the label of the browser). If
-- the response is not readable by the browser, the middleware sends a
-- 403 (unauthorized) response instead.
browserLabelGuard :: Middleware
browserLabelGuard hailsApp conf req = do
  response <- hailsApp conf req
  resultLabel <- getLabel
  trace (show resultLabel ++ " --> " ++ show (browserLabel conf) ++ "\n") $ do
  return $ if resultLabel `canFlowTo` (browserLabel conf)
             then response
             else Response status403 [] ""

-- | Adds the header @Content-Security-Policy@ to the response, if the
-- label of the computation does not flow to the public label,
-- 'dcPublic'.  The @default-src@ directive is set to the secrecy
-- component of the response label (if it is a disjunction
-- of principals). Currently, @'self'@ is always added to the
-- whitelist. An example may be:
--
-- > Content-Security-Policy: default-src 'self' http://google.com:80 https://a.lvh.me:3000;
--
guardSensitiveResp :: Middleware
guardSensitiveResp app config req = do
  response <- (flip removeResponseHeader) csp `liftM` app config req
  resultLabel <- getLabel
  return $ if resultLabel `canFlowTo` dcPublic
    then response
    else addResponseHeader response $
          ( csp
          , "default-src " <> headerVal resultLabel <> ";")
      where csp = "Content-Security-Policy"
            headerVal l =
              let secrecy     = dcSecrecy l
                  secrecySet  = cToSet secrecy
                  uriList     = Set.filter (isURI . S8.unpack) $ 
                                Set.map principalName $ 
                                dToSet $ head $ Set.elems secrecySet
              in if secrecy == cFalse || Set.size secrecySet > 1
                   then "\'none\'" -- false/conjunction
                   else S8.unwords $
                          "\'self\'":"\'unsafe-inline\'":(Set.toList uriList)

-- | Remove anything from the response that could cause inadvertant
-- declasification. Currently this only removes the @Set-Cookie@
-- header.
sanitizeResp :: Middleware
sanitizeResp hailsApp conf req = do
  response <- hailsApp conf req
  return $ foldr (\h r -> removeResponseHeader r h) response unsafeHeaders
   where unsafeHeaders = ["Set-Cookie"]

  

-- | Returns a secure Hails app such that the result 'Response' is guaranteed
-- to be safe to transmit to the client's browser. The definition is
-- straight forward from other middleware:
--
-- > secureApplication = 'browserLabelGuard'  -- Return 403, if user should not read
-- >                   . 'sanitizeResp'       -- Remove Cookies/CSP
-- >                   . 'guardSensitiveResp' -- Add CSP if not public
secureApplication :: Middleware
secureApplication = browserLabelGuard  -- Return 403, if user should not read
                  . sanitizeResp       -- Remove Cookies and X-Hails-Sensitive
                  . guardSensitiveResp -- Add CSP if not public

-- | Catch all exceptions thrown by middleware and return 500.
catchAllExceptions :: W.Middleware
catchAllExceptions app req = app req `catchError` (const $ return resp500)
    where resp500 = W.responseLBS status500 [] "App threw an exception"

--
-- Executing Hails applications
--

-- | Execute an application, safely filtering unsafe request headers,
-- overriding method posts,  catching all exceptions, and sanitizing
-- responses.
execHailsApplication :: W.Middleware -> Application -> W.Application
execHailsApplication authMiddleware app =
    catchAllExceptions
  . sanitizeReqMiddleware
  . methodOverridePost
  . authMiddleware
  $ \req -> hailsApplicationToWai app req

-- | Safely wraps a Hails 'Application' in a Wai 'W.Application' that can
-- be run by an application server. The application is executed with the
-- 'secureApplication' 'Middleware'. The function returns status 500 if
-- the Hails application throws an exception and the label of the
-- exception flows to the browser label (see 'browserLabelGuard'); if the
-- label does not flow, it responds with a 403.
--
-- All applications serve static content from a @\"static\"@ directory.
--
-- Note: this function assumes that the request has already been sanitized.
-- In most cases, you want to use 'execHailsApplication'.
hailsApplicationToWai :: Application -> W.Application
hailsApplicationToWai app0 req0 | isStatic req0 =
  -- Is static request, serve files:
  W.staticApp (W.defaultWebAppSettings "./") req0
                                | otherwise = do
  -- Not static request, serve dynamic content:
  -- Convert request to Hails request
  hailsRequest <- waiToHailsReq req0
  -- Extract browser/request configuration
  let conf = getRequestConf hailsRequest
  v <- liftIO $ do
    _ <- privInit (dcIntegrity (requestLabel conf))
    rc <- principalRC arena
    mkRCRefFromCNF (dcIntegrity (requestLabel conf)) =<< withRC1 rc hailsRequest (transfer trRequest)
  (result, dcState) <- liftIO $ tryDCDef conf $ do
    let lreq = LabeledTCB (requestLabel conf) v
    app conf lreq
  case result of
    Right response -> return $ hailsToWaiResponse response
    Left err -> do
      liftIO $ hPutStrLn stderr $ "App threw exception: " ++ show err
      return $
        if lioLabel dcState `canFlowTo` (browserLabel conf) then
          resp500
          else resp403
    where app = secureApplication app0
          isStatic req = case W.pathInfo req of
                           ("static":_) -> True
                           _            -> False
          resp403 = W.responseLBS status403 [] "" 
          resp500 = W.responseLBS status500 [] ""
          tryDCDef conf act = tryDC $ do
            putLIOStateTCB $ LIOState { lioLabel = dcPublic
                                     , lioClearance = browserLabel conf}
            act


--
-- Helper
--

arena :: Principal
arena = principalBS "#ARENA"

-- | Get the browser label (secrecy of the user), request label (integrity of
-- the user), and application privilege (minted with the app's cannonical name)
getRequestConf :: Request -> RequestConfig
getRequestConf req =
  let headers = requestHeaders req
      muserName = principalBS `fmap` lookup "x-hails-user" headers
      appName  = "@" `S8.append` (S8.takeWhile (/= '.') $ serverName req)
      appPriv = PrivTCB $ toCNF $ principalBS appName
  in RequestConfig
      { browserLabel = maybe (True %% True) (\userName -> userName %% True) muserName
      , requestLabel = maybe (True %% arena) (\userName -> True %% (arena /\ userName)) muserName
      , appPrivilege = appPriv }


