{-# LANGUAGE OverloadedStrings #-}
{- |

Generic definitions for authentication pipelines in Hails.
'requireLoginMiddleware' looks for the \"X-Hails-Login\" header from an
'Application's 'Response' and, if present, responds
to the user with an authentication request instead (e.g. a redirect to a login
page or an HTTP response with status 401). 

In addition, authentication middlewares for basic HTTP authentication (useful
in development environments) and federated (OpenID) authentication. In general,
authentication middlewares are expected to set the \"X-Hails-User\" header on
the request if it is from an authenticated user.

-}
module Hails.HttpServer.Auth
  ( requireLoginMiddleware
  , openIdAuth
  , devBasicAuth, authenticatedBasic
  ) where
import Blaze.ByteString.Builder (toByteString)
import Control.Monad.Trans.Resource
import Data.Time.Clock
import Data.ByteString.Base64
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Conduit (withManager)
import Network.HTTP.Types
import Network.Wai
import Web.Authenticate.OpenId
import Web.Cookie

-- | Helper method for implementing basic authentication. Given a 'Request'
-- returns a username-password pair from the basic authentication header if
-- present and valid.
authenticatedBasic :: Request -> Maybe (S8.ByteString, S8.ByteString)
authenticatedBasic req = do
  case lookup hAuthorization (requestHeaders req) of
    Nothing -> Nothing
    Just authStr
      | "Basic" `S8.isPrefixOf` authStr -> Nothing
      | otherwise ->
          let up = fmap (S8.split ':') $ decode $ S8.drop 6 authStr
          in case up of
            Right (user:pass:[]) -> Just (user, pass)
            _ -> Nothing


-- | Basic HTTP authentication middleware for development. Accepts any username
-- and password.
devBasicAuth :: String {- ^ Realm -} -> Middleware
devBasicAuth realm app0 req0 = do
  let resp = responseLBS status401
               [( "WWW-Authenticate"
                , S8.pack $ "Basic realm=\"" ++ realm ++ "\"")] ""
  let req = case authenticatedBasic req0 of
              Nothing -> req0
              Just (user, _) ->
                req0 { requestHeaders = (("X-Hails-User", user):(requestHeaders req0)) }
  requireLoginMiddleware (return resp) app0 req


requestToUri :: Request -> S8.ByteString -> S8.ByteString
requestToUri req path = S8.concat $
  [ "http"
  , if isSecure req then "s://" else "://"
  , serverName req
  , if serverPort req /= 80 && serverPort req /= 443 then portBS else ""
  , path ]
  where portBS = S8.concat [":", S8.pack . show $ serverPort req ]

openIdAuth :: T.Text {- ^ OpenID Provider -} -> Middleware
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
      let cookie = toByteString . renderSetCookie $ def
                     { setCookieName = "hails_session"
                     , setCookiePath = Just "/"
                     , setCookieValue = S8.pack . T.unpack . identifier . oirOpLocal $ oidResp }
      let redirectTo = fromMaybe "/" $ do
                        rawCookies <- lookup "Cookie" $ requestHeaders req0
                        lookup "redirect_to" $ parseCookies rawCookies
      return $ responseLBS status302 [ ("Set-Cookie", cookie)
                                     , ("Location", redirectTo)] ""
    _ -> do
      let req = fromMaybe req0 $ do
                  rawCookies <- lookup "Cookie" $ requestHeaders req0
                  user <- lookup "hails_session" $ parseCookies rawCookies
                  return $ req0 { requestHeaders =
                                    ("X-Hails-User", user):(requestHeaders req0)
                                }
      let redirectResp = do
          let returnUrl = T.pack . S8.unpack $ requestToUri req "/_hails/login"
          url <- withManager $ getForwardUrl openIdUrl returnUrl Nothing []
          let cookie = toByteString . renderSetCookie $ def
                         { setCookieName = "redirect_to"
                         , setCookiePath = Just "/_hails/"
                         , setCookieValue = rawPathInfo req }
          return $ responseLBS status302 [ ("Location", (S8.pack . T.unpack $ url))
                                         , ("Set-Cookie", cookie)] ""
      requireLoginMiddleware redirectResp app0 req

-- | If if app responds with header X-Hails-Login, respond with authentication
-- response (Basic Auth, redirect, etc...)
requireLoginMiddleware :: ResourceT IO Response -> Middleware
requireLoginMiddleware loginResp app0 req = do
  appResp <- app0 req
  case lookup "X-Hails-Login" (responseHeaders appResp) of
    Nothing -> do
      return appResp
    Just _ -> do
      case lookup "X-Hails-User" (requestHeaders req) of
        Nothing -> loginResp
        Just _ -> return appResp

responseHeaders :: Response -> ResponseHeaders
responseHeaders (ResponseFile _ hdrs _ _) = hdrs
responseHeaders (ResponseBuilder _ hdrs _) = hdrs
responseHeaders (ResponseSource _ hdrs _) = hdrs

