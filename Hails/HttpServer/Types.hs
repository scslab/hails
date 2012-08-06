module Hails.HttpServer.Types where

import qualified Network.Wai as W

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.List
import Network.Socket (SockAddr)
import qualified Network.HTTP.Types as H
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

data Response = Response H.Status H.ResponseHeaders L.ByteString

addResponseHeader :: Response -> H.Header -> Response
addResponseHeader (Response s hdrs body) hdr@(hname, _) = Response s (hdr:headers) body
  where headers = Prelude.filter (\(n, _) -> n /= hname) hdrs

hailsToWaiResponse :: Response -> W.Response
hailsToWaiResponse (Response stat rhd body) = W.responseLBS stat rhd body

data RequestConfig = RequestConfig { browserLabel :: DCLabel
                                   , requestLabel :: DCLabel
                                   , appPrivilege :: DCPrivTCB }

type Application = RequestConfig -> DCLabeled Request -> DC Response

type Middleware = Application -> Application

