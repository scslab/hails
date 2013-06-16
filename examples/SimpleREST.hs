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
server = mkRouter $ do
  routeTop (redirectTo "/users")
  routeName "users" $ do
    REST.index $ do
      req <- request >>= liftLIO . unlabel
      return $ okHtml $ fromString $
        "Welcome to " ++ (show $ serverName req) ++
        "<br/>Go to url: /users/:id/"
    REST.show $ do
      userId <- fromMaybe "" `liftM` queryParam "id"
      return $ ok "text/json" $ fromString $
        "{\"myid\": " ++ (show userId) ++ "}"
