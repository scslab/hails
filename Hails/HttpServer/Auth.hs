{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{- |

This module exports generic definitions for Wai-authentication pipelines
in Hails.  'requireLoginMiddleware' looks for the @X-Hails-Login@
header from an 'Application' \'s 'Response' and, if present, responds to
the user with an authentication request instead of the 'Application'
response (e.g., a redirect to a login page or an HTTP response with
status 401). 

Additionally, this module exports authentication 'Middleware's for basic HTTP
authentication, 'devBasicAuth', (useful in development environments)
and federated (OpenID) authentication, 'openIdAuth'. In general,
authentication 'Middleware's are expected to set the @X-Hails-User@
header on the request if it is from an authenticated user.

-}
module Hails.HttpServer.Auth
  ( requireLoginMiddleware
  -- * Production
  -- ** Persona (BrowserID)
  , personaAuth
  -- ** OpenID
  , openIdAuth
  -- ** Authenticate with external app
  , externalAuth
  -- * Development: basic authentication
  , devBasicAuth
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Blaze.ByteString.Builder (toByteString)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Time.Clock
import           Data.ByteString.Base64
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Digest.Pure.SHA
import           Network.HTTP.Conduit (withManager)
import           Network.HTTP.Types
import           Network.Wai
import           Web.Authenticate.BrowserId
import           Web.Authenticate.OpenId
import           Web.Cookie


-- | Basic HTTP authentication middleware for development. Accepts any username
-- and password.
devBasicAuth :: Middleware
devBasicAuth app0 req0 = do
  let resp = responseLBS status401
               [( "WWW-Authenticate", "Basic realm=\"Hails development.\"")] ""
  let req = case getBasicAuthUser req0 of
              Nothing -> req0
              Just user -> req0 { requestHeaders = ("X-Hails-User", user)
                                                 : requestHeaders req0 }
  requireLoginMiddleware (return resp) app0 req

-- | Authentica user with Mozilla's persona.
-- If the @X-Hails-Persona-Login@ header is set, this intercepts the
-- request and verifies the supplied identity assertion, supplied in the
-- request body.
--
-- If the authentication is successful, set the @_hails_user@ and
-- @_hails_user_hmac@ cookies to identify the user. The former
-- contains the user email address, the latter contains the MAC that is
-- used for verifications in later requests.
--
-- If the @X-Hails-Persona-Logout@ header is set, this intercepts the
-- request and deletes the aforementioned cookies.
-- 
-- If the app wishes the user to authenticate (by setting @X-Hails-Login@)
-- this redirects to @audience/login@ -- where the app can call
-- @navigator.request()@.
--
personaAuth :: L8.ByteString -> Text -> Middleware
personaAuth key audience app0 req0 = do
   case () of
    _ | doLogin -> do
        assertion <- S8.concat `liftM` (requestBody req0 C.$$ C.consume)
        muser <- withManager $ checkAssertion audience (T.decodeUtf8 $ assertion)
        case muser of
          Nothing -> return $ responseLBS status401 [] ""
          Just usr -> let hmac   = T.pack $ showDigest $ hmacSha1 key
                                            (L8.fromStrict . T.encodeUtf8 $ usr)
                      in return $ responseLBS status200
                            [ ("Set-Cookie", setCookie "_hails_user" usr) 
                            , ("Set-Cookie", setCookie "_hails_user_hmac" hmac)]
                            ""
    _ | doLogout -> return $ responseLBS status200
                              [ ("Set-Cookie", delCookie "_hails_user")
                              , ("Set-Cookie", delCookie "_hails_user_hmac")]
                              ""
    _           ->
      let mauth = do cookies <- parseCookies `liftM`
                                     (lookup "Cookie" $ requestHeaders req0)
                     usr   <- lookup "_hails_user" cookies
                     hmac0 <- lookup "_hails_user_hmac" cookies
                     let hmac1 = showDigest $ hmacSha1 key $ L8.fromStrict usr
                     return (usr, hmac0 == S8.pack hmac1)
          req = case mauth of
                  Just (usr, True) -> req0 { requestHeaders =
                                               ("X-Hails-User", usr)
                                               :(requestHeaders req0) }
                  _ -> req0
      in requireLoginMiddleware (return $ respRedir req) app0 req
  where doLogin  = isJust $ lookup "X-Hails-Persona-Login"  $ requestHeaders req0
        doLogout = isJust $ lookup "X-Hails-Persona-Logout" $ requestHeaders req0
        setCookie n v = toByteString . renderSetCookie $ def {
                           setCookieName = n
                         , setCookiePath = Just "/"
                         , setCookieValue = T.encodeUtf8 v }
        delCookie n = toByteString . renderSetCookie $ def {
                         setCookieName = n
                       , setCookiePath = Just "/"
                       , setCookieValue = "deleted"
                       , setCookieExpires = Just $ UTCTime (toEnum 0) 0 }
        respRedir req =
          let cookie =  toByteString . renderSetCookie $ def
                         { setCookieName = "redirect_to"
                         , setCookiePath = Just "/"
                         , setCookieValue = rawPathInfo req }
          in responseLBS status302
              [ ("Set-Cookie", cookie)
              , ("Location", (T.encodeUtf8 audience) `S8.append` "/login") ] ""

-- | Perform OpenID authentication.
openIdAuth :: T.Text -- ^ OpenID Provider 
           -> Middleware
openIdAuth openIdUrl app0 req0 = do
  case pathInfo req0 of
    "_hails":"logout":_ -> do
      let cookie = toByteString . renderSetCookie $ def
                      { setCookieName = "hails_session"
                      , setCookiePath = Just "/"
                      , setCookieValue = "deleted"
                      , setCookieExpires = Just $ UTCTime (toEnum 0) 0}
      let redirectTo = fromMaybe "/" $ lookup "Referer" $ requestHeaders req0
      return $ responseLBS status302 [ ("Set-Cookie", cookie)
                                     , ("Location", redirectTo)] ""
    "_hails":"login":_ -> do
      let qry = map (\(n,v) -> (n, fromJust v)) $ filter (isJust . snd) $
                  parseQueryText $ rawQueryString req0
      oidResp <- withManager $ authenticateClaimed qry
      liftIO $ print $ oirParams oidResp
      let cookie = toByteString . renderSetCookie $ def
                     { setCookieName = "hails_session"
                     , setCookiePath = Just "/"
                     , setCookieValue = S8.pack . T.unpack . identifier . oirOpLocal $ oidResp }
      let redirectTo = fromMaybe "/" $ do
                        rawCookies <- lookup "Cookie" $ requestHeaders req0
                        lookup "redirect_to" $ parseCookies rawCookies
      return $ responseLBS status200 ([ ("Set-Cookie", cookie)
                                      , ("Location", redirectTo)])
                                      (L8.pack $ show qry)
    _ -> do
      let req = fromMaybe req0 $ do
                  rawCookies <- lookup "Cookie" $ requestHeaders req0
                  user <- lookup "hails_session" $ parseCookies rawCookies
                  return $ req0 { requestHeaders =
                                    ("X-Hails-User", user):(requestHeaders req0)
                                }
      let redirectResp = do
          let returnUrl = T.pack . S8.unpack $ requestToUri req "/_hails/login"
          url <- withManager $ getForwardUrl openIdUrl returnUrl Nothing
                                [ ("openid.ns.ax", "http://openid.net/srv/ax/1.0")
                                , ("openid.ax.mode", "fetch_request")
                                , ("openid.ax.type.email", "http://schema.openid.net/contact/email")
                                , ("openid.ax.required", "email")]
          let cookie = toByteString . renderSetCookie $ def
                         { setCookieName = "redirect_to"
                         , setCookiePath = Just "/_hails/"
                         , setCookieValue = rawPathInfo req }
          return $ responseLBS status302 [ ("Location", (S8.pack . T.unpack $ url))
                                         , ("Set-Cookie", cookie)] ""
      requireLoginMiddleware redirectResp app0 req

-- | Executes the app and if the app 'Response' has header
-- @X-Hails-Login@ and the user is not logged in, respond with an
-- authentication response (Basic Auth, redirect, etc.)
requireLoginMiddleware :: ResourceT IO Response -> Middleware
requireLoginMiddleware loginResp app0 req = do
  appResp <- app0 req
  if hasLogin appResp && notLoggedIn
    then loginResp
    else return appResp
  where hasLogin r = "X-Hails-Login" `isIn` responseHeaders r
        notLoggedIn = not $ "X-Hails-User" `isIn` requestHeaders req
        isIn n xs = isJust $ lookup n xs

-- | Get the hreaders from a response.
responseHeaders :: Response -> ResponseHeaders
responseHeaders (ResponseFile _ hdrs _ _) = hdrs
responseHeaders (ResponseBuilder _ hdrs _) = hdrs
responseHeaders (ResponseSource _ hdrs _) = hdrs

--
-- Helpers
--

-- | Helper method for implementing basic authentication. Given a
-- 'Request' returns the usernamepair from the basic authentication
-- header if present.
getBasicAuthUser :: Request -> Maybe S8.ByteString
getBasicAuthUser req = do
  authStr <- lookup hAuthorization $ requestHeaders req
  unless ("Basic" `S8.isPrefixOf` authStr) $ fail "Not basic auth."
  let up = fmap (S8.split ':') $ decode $ S8.drop 6 authStr
  case up of
     Right (user:_:[]) -> return user
     _ -> fail "Malformed basic auth header."

-- | Given a request and path, extract the scheme,
-- hostname and port from the request and createand a URI
-- @scheme://hostname[:port]/path@.
requestToUri :: Request -> S8.ByteString -> S8.ByteString
requestToUri req path = S8.concat $
  [ "http"
  , if isSecure req then "s://" else "://"
  , serverName req
  , if serverPort req `notElem` [80, 443] then portBS else ""
  , path ]
  where portBS = S8.pack $ ":" ++ show (serverPort req)


-- Cookie authentication
--

-- | Use an external authentication service that sets cookies.
-- The cookie names are @_hails_user@, whose contents contains the
-- @user-name@, and @_hails_user_hmac@, whose contents contains
-- @HMAC-SHA1(user-name)@. This function simply checks that the cookie
-- exists and the MAC'd user name is correct. If this is the case, it
-- returns a request with the cookie removed and @x-hails-user@ header
-- set. Otherwies the original request is returned.
-- The login service retuns a redirect (to the provided url).
-- Additionally, cookie @_hails_refer$ is set to the current
-- URL (@scheme://domain:port/path@).
externalAuth :: L8.ByteString -> String -> Middleware
externalAuth key url app req = do
  let mreqAuth = do
        cookieHeaders <- lookup hCookie $ requestHeaders req
        let cookies = parseCookies cookieHeaders
        mac0 <- fmap (read . S8.unpack) $ lookup "_hails_user_hmac" cookies
        user <- fmap (read . S8.unpack) $ lookup "_hails_user" cookies
        let mac1 = showDigest $ hmacSha1 key (lazyfy user)
        if S8.unpack mac0 == mac1
          then Just $ req { requestHeaders = ("X-Hails-User", user)
                                               : requestHeaders req }
          else Nothing
      req0 = maybe req id mreqAuth
  requireLoginMiddleware redirectResp app req0
  where redirectResp = return $ responseLBS status302
                          [(hLocation, S8.pack url)] ""
        --
        lazyfy = L8.fromChunks . (:[])

