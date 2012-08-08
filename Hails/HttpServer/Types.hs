{- |

This module defines types for Hails including the base type, 'Application',
which, at a high level, is a function from 'Request' to 'Response' in the 'DC'
Monad.

-}
module Hails.HttpServer.Types
  ( Application
  , Request(..), RequestConfig(..), waiToHailsReq
  , Response(..), hailsToWaiResponse
  , addResponseHeader
  , removeResponseHeader
  , Middleware
  ) where

import qualified Network.Wai as W

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.List
import Network.Socket (SockAddr)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Data.Text (Text)
import LIO.DCLabel

-- | Information on the request sent by the client.
data Request = Request
  {  requestMethod  :: H.Method
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
  ,  requestHeaders :: H.RequestHeaders
  -- | Was this request made over an SSL connection?
  ,  isSecure       :: Bool
  -- | The client\'s host information.
  ,  remoteHost     :: SockAddr
  -- | Path info in individual pieces- the url without a hostname/port and without a query string, split on forward slashes,
  ,  pathInfo       :: [Text]
  -- | Parsed query string information
  ,  queryString    :: H.Query
  ,  requestBody    :: L.ByteString
  }


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

-- | Contains the HTTP response 'Status', a list of 'Header's and the response
-- body as a 'L.ByteString'
data Response = Response H.Status H.ResponseHeaders L.ByteString

-- | Add/replace a 'H.Header' to the 'Response'
addResponseHeader :: Response -> H.Header -> Response
addResponseHeader (Response s hdrs body) hdr@(hname, _) = Response s (hdr:headers) body
  where headers = Prelude.filter ((/= hname) . fst) hdrs

-- | Remove a header (if it exists) from the 'Response'
removeResponseHeader :: Response -> H.HeaderName -> Response
removeResponseHeader (Response s hdrs body) hname = Response s headers body
  where headers = Prelude.filter ((/= hname) . fst) hdrs

-- | Convert a Hails 'Response' to a WAI 'W.Response'
hailsToWaiResponse :: Response -> W.Response
hailsToWaiResponse (Response stat rhd body) = W.responseLBS stat rhd body

-- | The settings with which the application will run:
--
--   * The label of the browser the reponse will be sent to
--
--   * The label of the incoming request (with the logged in user's integrity)
--
--   * A privilege minted for the app
data RequestConfig = RequestConfig { browserLabel :: DCLabel
                                   , requestLabel :: DCLabel
                                   , appPrivilege :: DCPriv }

-- | Base Hails type implemented by untrusted applications.
type Application = RequestConfig -> DCLabeled Request -> DC Response

-- | Convenience type for middleware components.
type Middleware = Application -> Application

