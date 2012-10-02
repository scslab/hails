module Hails.HttpServer.Types (
  -- * Requests
    Request(..)
  -- * Responses
  , Response(..)
  , addResponseHeader, removeResponseHeader
  -- * Applications and middleware
  , Application, RequestConfig(..)
  , Middleware) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import           Network.Socket (SockAddr)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H

import           LIO.DCLabel

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
