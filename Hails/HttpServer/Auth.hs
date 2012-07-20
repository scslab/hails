{-# LANGUAGE OverloadedStrings #-}
module Hails.HttpServer.Auth where

import Network.HTTP.Types
import Network.Wai

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

