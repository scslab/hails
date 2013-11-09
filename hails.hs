{-# LANGUAGE CPP #-}
module Main where

--  #ifdef SERVER_MODULE
--  import SERVER_MODULE
--  #else
--  import Server
--  #endif

import Hails.HttpServer
import Hails.HttpServer.Auth

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import qualified Data.ByteString.Lazy.Char8 as L8

import           LIO

server :: Application
server _ lreq = do
  req <- unlabel lreq
  return $ Response ok200 [] (L8.pack "w00t")

main :: IO ()
main = do
    let logMiddleware = logStdoutDev
        authMiddleware = devBasicAuth
    runSettings (defaultSettings { settingsPort = 8888 }) $
        logMiddleware $ execHailsApplication authMiddleware server
