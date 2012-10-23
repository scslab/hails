{-# LANGUAGE OverloadedStrings #-}
module SimpleFrank (server) where

import           Data.String
import           Data.Maybe
import           Control.Monad

import           LIO
import           Hails.HttpServer.Types
import           Hails.Web
import qualified Hails.Web.Frank as Frank

server :: Application
server = mkRouter $ do
    Frank.get "/users" $ do
      req <- request >>= unlabel
      return $ okHtml $ fromString $
        "Welcome Home " ++ (show $ serverName req)
    Frank.get "/users/:id" $ do
      userId <- fromMaybe "" `liftM` queryParam "id"
      return $ ok "text/json" $ fromString $
        "{\"myid\": " ++ (show userId) ++ "}"
