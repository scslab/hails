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
  , devBasicAuth, authenticatedBasic
  ) where

import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import Network.Wai

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
              Nothing -> req
              Just (user, _) ->
                req { requestHeaders = (("X-Hails-User", user):(requestHeaders req)) }
  requireLoginMiddleware resp app0 req

-- | If if app responds with header X-Hails-Login, respond with authentication
-- response (Basic Auth, redirect, etc...)
requireLoginMiddleware :: Response -> Middleware
requireLoginMiddleware loginResp app0 req = do
  appResp <- app0 req
  case lookup "X-Hails-Login" (responseHeaders appResp) of
    Nothing -> return appResp
    Just _ -> return loginResp

responseHeaders :: Response -> ResponseHeaders
responseHeaders (ResponseFile _ hdrs _ _) = hdrs
responseHeaders (ResponseBuilder _ hdrs _) = hdrs
responseHeaders (ResponseSource _ hdrs _) = hdrs

