module Hails.HttpServer.Types where

import qualified Network.Wai as W

import qualified Data.ByteString as B
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
  ,  rawPathInfo    :: B.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value- modify queryString instead.
  ,  rawQueryString :: B.ByteString
  -- | Generally the host requested by the user via the Host request header.
  -- Backends are free to provide alternative values as necessary. This value
  -- should not be used to construct URLs.
  ,  serverName     :: B.ByteString
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
waiToHailsReq (W.Request m ver rp rqs sn sp rhs isS rh pi qs rb _) = do
  body <- fmap L.fromChunks $ rb $$ consume
  return $ Request m ver rp rqs sn sp rhs isS rh pi qs body

data Response = Response H.Status H.ResponseHeaders L.ByteString

addResponseHeader :: Response -> H.Header -> Response
addResponseHeader (Response s hdrs body) hdr@(name, val) = Response s (hdr:headers) body
  where headers = Prelude.filter (\(n, _) -> n /= name) hdrs

hailsToWaiResponse :: Response -> W.Response
hailsToWaiResponse (Response stat rhd body) = W.responseLBS stat rhd body

type Application = DCPrivTCB -> DCLabeled Request -> DC Response

type Middleware = Application -> Application

