{-# LANGUAGE OverloadedStrings #-}
module Hails.HttpServer.Auth where

import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import Network.Wai

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


devBasicAuth :: String -> Middleware
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

