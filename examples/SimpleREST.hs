{-# LANGUAGE OverloadedStrings #-}
module SimpleREST (server) where

import           Data.String
import           Data.Maybe
import           Control.Monad

import           LIO
import           Hails.HttpServer.Types
import           Hails.Web
import qualified Hails.Web.REST as REST

server :: Application
server = mkRouter $ routeName "users" $ do
  REST.index $ do
    req <- request >>= unlabel
    return $ okHtml $ fromString $
      "Welcome Home " ++ (show $ serverName req)
  REST.show $ do
    userId <- fromMaybe "" `liftM` queryParam "id"
    return $ ok "text/json" $ fromString $
      "{\"myid\": " ++ (show userId) ++ "}"
