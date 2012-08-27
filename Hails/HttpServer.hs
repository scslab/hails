{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{- |

This module exports the core of the Hails HTTP server. Specifically it
defines basic types, such as HTTP 'Request' and 'Response', used by
the Hails web server and untrusted Hails 'Application's. 

At a high level, a Hails 'Application', is a function from 'Request'
to 'Response' in the 'DC' monad. Every application response is
sanitized and sanity checked with the 'secureApplication'
'Middleware'.

Hails uses Wai, and as such we provide two functions for converting
Hails 'Application's to Wai 'W.Applicatoin's: '
'devHailsApplication' used to execute Hails apps in development
mode, and 'hailsApplicationToWai' that should be used in production
with an authentication service from "Hails.HttpServer.Auth".

-}
module Hails.HttpServer (
  -- * Requests
    Request(..)
  -- * Responses
  , Response(..)
  , addResponseHeader, removeResponseHeader
  -- * Applications and middleware
  , Application, RequestConfig(..)
  , Middleware
  -- ** Execute Hails application in development mode
  , devHailsApplication
  -- ** Execute Hails application
  , hailsApplicationToWai
  -- ** Middleware used by Hails
  , browserLabelGuard
  , guardSensitiveResp 
  , sanitizeResp
  -- * Network types
  , module Network.HTTP.Types
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Conduit.List

import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (fromException)


import           Network.Socket (SockAddr)
import           Network.HTTP.Types
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wai as W

import           LIO
import           LIO.TCB
import           LIO.DCLabel
import           LIO.DCLabel.Privs.TCB
import           LIO.Labeled.TCB

import           Hails.HttpServer.Auth

import           System.IO

--
-- Request
--

-- | A request sent by the end-user.
data Request = Request {  
  -- | HTTP Request (e.g., @GET@, @POST@, etc.).
  requestMethod  :: H.Method
  -- | HTTP version (e.g., 1.1 or 1.0).
  ,  httpVersion    :: H.HttpVersion
  -- | Extra path information sent by the client.
  ,  rawPathInfo    :: S.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value- modify queryString instead.
  ,  rawQueryString :: S.ByteString
  -- | Generally the host requested by the user via the Host request header.
  -- Backends are free to provide alternative values as necessary. This value
  -- should not be used to construct URLs.
  ,  serverName     :: S.ByteString
  -- | The listening port that the server received this request on. It is
  -- possible for a server to listen on a non-numeric port (i.e., Unix named
  -- socket), in which case this value will be arbitrary. Like 'serverName',
  -- this value should not be used in URL construction.
  ,  serverPort     :: Int
  -- | The request headers.
  ,  requestHeaders :: H.RequestHeaders
  -- | Was this request made over an SSL connection?
  ,  isSecure       :: Bool
  -- | The client\'s host information.
  ,  remoteHost     :: SockAddr
  -- | Path info in individual pieces- the url without a hostname/port
  -- and without a query string, split on forward slashes,
  ,  pathInfo       :: [Text]
  -- | Parsed query string information
  ,  queryString    :: H.Query
  -- | Lazy ByteString containing the request body.
  ,  requestBody    :: L.ByteString
  } deriving Show


-- | Convert a WAI 'W.Request' to a Hails 'Request' by consuming the body into
-- a 'L.ByteString'.
waiToHailsReq :: W.Request -> ResourceT IO Request
waiToHailsReq req = do
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
                   , requestBody = body }
--
-- Response
--

-- | A response sent by the app.
data Response = Response {
  -- | Response status
    respStatus :: H.Status
  -- | Response headers
  , respHeaders :: H.ResponseHeaders 
  -- | Response body
  , respBody :: L.ByteString
  } deriving Show

-- | Add/replace a 'H.Header' to the 'Response'
addResponseHeader :: Response -> H.Header -> Response
addResponseHeader resp hdr@(hname, _) = resp { respHeaders = hdr:headers }
  where headers = List.filter ((/= hname) . fst) $ respHeaders resp

-- | Remove a header (if it exists) from the 'Response'
removeResponseHeader :: Response -> H.HeaderName -> Response
removeResponseHeader resp hname = resp { respHeaders = headers }
  where headers = List.filter ((/= hname) . fst) $ respHeaders resp

-- | Convert a Hails 'Response' to a WAI 'W.Response'
hailsToWaiResponse :: Response -> W.Response
hailsToWaiResponse (Response stat rhd body) = W.responseLBS stat rhd body



--
-- Application & middleware
--

-- | The settings with which the app will run.
data RequestConfig = RequestConfig {
  -- | The label of the browser the reponse will be sent to.
    browserLabel :: DCLabel
  -- | The label of the incoming request (with the logged in user's integrity).
  , requestLabel :: DCLabel
  -- | A privilege minted for the app.
  , appPrivilege :: DCPriv
  } deriving Show

-- | Base Hails type implemented by untrusted applications.
type Application = RequestConfig -> DCLabeled Request -> DC Response

-- | Convenience type for middleware components.
type Middleware = Application -> Application

-- | Hails 'Middleware' that ensures the 'Response' from the
-- application is readable by the client's browser (as determined by the
-- result label of the app computation and the label of the browser). If
-- the response is not readable by the browser, the middleware sends a
-- 403 (unauthorized) response instead.
browserLabelGuard :: Middleware
browserLabelGuard hailsApp conf req = do
  response <- hailsApp conf req
  resultLabel <- getLabel
  return $ if resultLabel `canFlowTo` (browserLabel conf)
             then response
             else Response status403 [] ""

-- | Adds the header @X-Hails-Sensitive: Yes@ to the response if the
-- label of the computation does not flow to the public label, 'dcPub'
guardSensitiveResp :: Middleware
guardSensitiveResp happ p req = do
  response <- happ p req
  resultLabel <- getLabel
  return $ if resultLabel `canFlowTo` dcPub
            then response
            else addResponseHeader response ("X-Hails-Sensitive", "Yes")

-- | Remove anything from the response that could cause inadvertant
-- declasification. Currently this only removes the @Set-Cookie@
-- header.
sanitizeResp :: Middleware
sanitizeResp hailsApp conf req = do
  response <- hailsApp conf req
  return $ removeResponseHeader response "Set-Cookie"
  

-- | Returns a secure Hails app such that the result 'Response' is guaranteed
-- to be safe to transmit to the client's browser. The definition is
-- straight forward from other middleware:
--
-- > secureApplication = 'browserLabelGuard'  -- Return 403, if user should not read
-- >                   . 'guardSensitiveResp' -- Add X-Hails-Sensitive if not public
-- >                   . 'sanitizeResp'       -- Remove Cookies
secureApplication :: Middleware
secureApplication = browserLabelGuard  -- Return 403, if user should not read
                  . guardSensitiveResp -- Add X-Hails-Sensitive if not public
                  . sanitizeResp       -- Remove Cookies

--
-- Executing Hails applications
--

-- | A default Hails handler for development environments. Safely runs
-- a Hails 'Application', using basic HTTP authentication for
-- authenticating users.  Note: authentication will accept any
-- username/password pair, it is solely used to set the user-name.
devHailsApplication :: Application -> W.Application
devHailsApplication = devBasicAuth . hailsApplicationToWai


-- | Safely wraps a Hails 'Application' in a Wai 'W.Application' that can
-- be run by an application server. The application is executed with the
-- 'secureApplication' 'Middleware'. The function returns status 500 if
-- the Hails application throws an exception and the label of the
-- exception flows to the browser label (see 'browserLabelGuard'); if the
-- label does not flow, it responds with a 403.
hailsApplicationToWai :: Application -> W.Application
hailsApplicationToWai app0 req0 = do
  -- Convert request to Hails request
  hailsRequest <- waiToHailsReq req0
  -- Extract browser/request configuration
  let conf = getRequestConf hailsRequest
  result <- liftIO $ paranoidDC' conf $ do
    let lreq = labelTCB (requestLabel conf) hailsRequest
    app conf lreq
  case result of
    Right (response,_) -> return $ hailsToWaiResponse response
    Left err -> do
      liftIO $ hPutStrLn stderr $ "App threw exception: " ++ show err
      return $ case fromException err of
        Just (LabeledExceptionTCB l _) -> 
          -- as in browserLabelGuard :
          if l `canFlowTo` (browserLabel conf)
            then resp500 else resp403 
        _ -> resp500
    where app = secureApplication app0
          resp403 = W.responseLBS status403 [] "" 
          resp500 = W.responseLBS status500 [] ""
          paranoidDC' conf act =
            paranoidLIO act $ LIOState { lioLabel = dcPub
                                       , lioClearance = browserLabel conf}



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


