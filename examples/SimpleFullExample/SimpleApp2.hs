{-# LANGUAGE OverloadedStrings #-}
module SimpleApp2 (server) where

import Prelude hiding (lookup)
import Data.String
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import LIO
import Hails.Web
import qualified Hails.Web.Frank as Frank
import Hails.HttpServer
import Hails.Database

import SimplePolicyModule

server :: Application
server = mkRouter $ do
  Frank.get "/" $ do
    req <- request >>= liftLIO . unlabel
    return $ okHtml $ fromString $
      "Welcome to " ++ (show $ serverName req) ++
      "<h1>Store:</h1>"++
      "<iframe src=\"/static/store.html\"></iframe>"++
      "<h1>Fetch: </h1>"++
      "<iframe src=\"/static/fetch.html\"></iframe>"
  Frank.post "/store" $ do
    doc <- include ["key","val"] `liftM` hsonRequest
    if length doc /= 2
      then respond badRequest
      else do liftLIO $ withStorePolicyModule $ insert "store" doc
              respond $ redirectTo $ "/store/" ++ ("key" `at` doc)
  Frank.get "/store" $ do
    mk <- (fmap S8.unpack) `liftM` queryParam "key"
    respond $ maybe badRequest (\k -> redirectTo $ "/store/" ++ k) mk
  Frank.get "/store/:key" $ do
    mk <- queryParam "key"
    case mk of
      Nothing -> return notFound
      Just k -> do
       mlv <- liftLIO $ withStorePolicyModule $ do
                          findOne $ select ["key" -: k] "store"
       case mlv of
         Nothing -> return notFound
         Just lv -> do v <- liftLIO $ unlabel lv
                       return $ okHtml $ L8.pack $ "val" `at` v
    where hsonRequest :: Controller Document
          hsonRequest = request >>= labeledRequestToHson >>= (liftLIO . unlabel)
