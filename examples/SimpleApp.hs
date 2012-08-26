{-# LANGUAGE Safe #-}
module SimpleApp (server) where

import           Hails.HttpServer

import qualified Data.ByteString.Lazy.Char8 as L8

server :: Application
server _ _ = return $
  Response ok200 [] (L8.pack "w00t")
