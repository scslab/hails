{-# LANGUAGE OverloadedStrings #-}
module SimpleApp (server) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           LIO
import           LIO.DCLabel
import           Hails.HttpServer
import           Hails.Data.Hson

server :: Application
server _ lreq = do
  req <- unlabel lreq
  ldoc <- labeledRequestToHson lreq
  doc <- unlabel ldoc
  case pathInfo req of
    ("login":_) -> return $
      Response temporaryRedirect307 [("x-hails-login",""),(hLocation,"/")] ""
    ("taint":_) -> do
      ccur <- getClearance
      let url = "http://www.google.com:80" :: String 
          lbl = ccur `glb`dcLabel (toComponent url) dcTrue
      taint lbl
      return $ Response ok200 [] $ topHtml req
    _ -> return $
      Response ok200 [] $ topHtml req

topHtml :: Request -> L8.ByteString
topHtml req = L8.pack $ 
  "<html>\
  \ <head>\
  \<title>Simple post form example</title>\
  \</head>\
  \ <body>\
  \  <h1>1. Login</h1>\
  \  <a href=\"/login\">login</a>\
  \  <h1>2. Taint</h1>\
  \  <a href=\"/taint\">taint</a>\
  \  <img src=\"http://www.google.com/images/srpr/logo3w.png\">\
  \  </a>\
  \  <img src=\"http://www.haskell.org/wikistatic/haskellwiki_logo.png\">\
  \  </a>\
  \  </a>\
  \  <img src=\"http://holumbus.fh-wedel.de/hayoo/hayoo.png\">\
  \  </a>\
  \  <h1>Remote images</h1>\
  \  <h1>Request </h1>"
  ++"<div>" ++ show req ++ "</div>"
  ++ " </body>\
  \</html>"
