{-# LANGUAGE OverloadedStrings #-}
module SimpleApp (server) where

import           Data.Maybe
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Monad

import           LIO
import           Hails.HttpServer
import           Hails.Database

import           SimplePolicyModule
server :: Application
server _ lreq = do
  req <- unlabel lreq
  resp <- case pathInfo req of
    ("insert":_) -> do
      let es :: [(String,String)]
          es = map (\(k,mv) -> (S8.unpack k,S8.unpack $ fromJust mv)) $
               filter (isJust . snd) $ queryString req
      withStorePolicyModule $ forM_ es $ \(k,v) ->
        insert_ "store" (["key" -: k, "val" -: v] :: HsonDocument)
      return $ "Inserted" ++ show es
    ("fetch":_)  -> do
      let es = map (S8.unpack . fst) $
               filter (isNothing . snd) $ queryString req
      lmrs <- withStorePolicyModule $ forM es $ \k ->
        findOne (select ["key" -: k] "store")
      rs <- forM (filter isJust lmrs) (unlabel . fromJust)
      return $ "Fetched" ++ show rs
    [] -> return $ "Welcome to the simple key-value store!\n" ++ use
    _ -> return $ "Unrecognized query. Expecting:\n" ++ use
  return $ Response ok200 [] (L8.pack resp)
    where use = " Insert: /insert?key1=val1,key2=val2,..\n"
             ++ " Fetch:  /fetch?key1,key2\n"
