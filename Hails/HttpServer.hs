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
  module Hails.HttpServer.Types
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
  -- * Converting labeled requests
  , requestToDocument
  , labeledRequestToLabeledDocument
  ) where

import qualified Data.List as List
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Conduit.List

import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (fromException)


import           Network.HTTP.Types
import qualified Network.Wai as W
import qualified Network.Wai.Application.Static as W

import           LIO
import           LIO.TCB
import           LIO.DCLabel
import           LIO.DCLabel.Core (principalName)
import           LIO.DCLabel.Privs.TCB
import           LIO.Labeled.TCB

import           Hails.HttpServer.Auth
import           Hails.HttpServer.Types
import           Hails.Data.Hson hiding (lookup)

import           System.IO

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
  return $ if resultLabel `canFlowTo` (browserLabel conf)
             then response
             else Response status403 [] ""

-- | Adds the header @X-Hails-Label@ to the response. If the
-- label of the computation does not flow to the public label,
-- 'dcPub', the JSON field @isPublic@ is set to @true@, otherwise
-- it is set to @true@ and the JSON @label@ is set to the secrecy
-- component of the response label (if it is a disjunction
-- of principals is added). An example may be:
--
-- > X-Hails-Label = { isPublic: true }
-- 
-- or
--
-- > X-Hails-Label = { isPublic: false, label : ["http://google.com:80", "alice"] }
--
guardSensitiveResp :: Middleware
guardSensitiveResp happ p req = do
  response <- happ p req
  resultLabel <- getLabel
  return $ addResponseHeader response $ 
    ("X-Hails-Label", S8.pack $
      if resultLabel `canFlowTo` dcPub
        then "{\"isPublic\": true}"
        else "{\"isPublic\": false, \"label\": [" ++ mkClientLabel resultLabel ++ "]}")
      where mkClientLabel l = let s  = dcSecrecy l
                                  cs = toList s
                              in if s == dcFalse || length cs /= 1
                                   then ""
                                   else List.intercalate ", " $ 
                                        List.map (show . S8.unpack . principalName) $
                                        List.head cs

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
--
-- All applications serve static content from a @\"static\"@ directory.
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
          isStatic req = case W.pathInfo req of
                           ("static":_) -> True
                           _            -> False
          resp403 = W.responseLBS status403 [] "" 
          resp500 = W.responseLBS status500 [] ""
          paranoidDC' conf act =
            paranoidLIO act $ LIOState { lioLabel = dcPub
                                       , lioClearance = browserLabel conf}


--
--
--

-- | Convert a labeled request to a labeled document
labeledRequestToLabeledDocument :: DCLabeled Request -> DCLabeled HsonDocument
labeledRequestToLabeledDocument lreq =
  labelTCB (labelOf lreq) (requestToDocument . unlabelTCB $ lreq)

-- | Convert a form post from a request  into a document.
-- Each field value is a 'BsonBlob', hence further \"casting\"
-- may be necessary. Note that if a field with the same name is found,
-- the values are grouped into a 'BsonArray'.
requestToDocument :: Request -> HsonDocument
requestToDocument req = groupFields $ List.map itemToField $ parseQuery body
  where body = strictify $ requestBody req
        --
        itemToField (n, mv) = let nt = decodeUtf8 n
                                  hv = maybe BsonNull (BsonBlob . Binary) mv
                              in nt -: hv
        --
        strictify = S.concat . L.toChunks
        --
        groupFields :: HsonDocument -> HsonDocument
        groupFields xs =
          let gs = List.groupBy (\x y -> fieldName x == fieldName y) xs
          in List.concat $ List.map groupFunc gs
        groupFunc xs = let n = fieldName . List.head $ xs
                           ys :: [BsonValue]
                           ys = List.map (\(HsonField _ (HsonValue x)) -> x) xs
                       in [n -: ys]


--
-- Helper
--

-- | Get the browser label (secrecy of the user), request label (integrity of
-- the user), and application privilege (minted with the app's cannonical name)
getRequestConf :: Request -> RequestConfig
getRequestConf req =
  let headers = requestHeaders req
      userName  = toComponent `fmap` lookup "x-hails-user" headers
      appName  = '@' : (S8.unpack . S8.takeWhile (/= '.') $ serverName req)
      appPriv = DCPrivTCB $ toComponent appName
  in RequestConfig
      { browserLabel = maybe dcPub (\un -> dcLabel un anybody) userName
      , requestLabel = maybe dcPub (\un -> dcLabel anybody un) userName
      , appPrivilege = appPriv }


