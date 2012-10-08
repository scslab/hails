{-# LANGUAGE OverloadedStrings #-}
module SimpleParams (server) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           LIO
import           LIO.DCLabel
import           Hails.HttpServer
import           Hails.Data.Hson

server :: Application
server _ lreq = do
  req <- unlabel lreq
  let ldoc = labeledRequestToLabeledDocument lreq
  doc <- unlabel ldoc
  return $ case pathInfo req of
    ("login":_) -> Response temporaryRedirect307 
                            [("x-hails-login",""),(hLocation,"/")] ""
    _ -> Response ok200 [] $ topHtml (labelOf lreq, req) (labelOf ldoc, doc)

topHtml :: (DCLabel, Request) -> (DCLabel, Document) -> L8.ByteString
topHtml (lr, req) (ld, doc) = L8.pack $ 
  "<html>\
  \ <head> <title>Simple post form example</title></head>\
  \ <body>\
  \  <h1>Login</h1>\
  \  <a href=\"/login\">login</a>\
  \  <h1>Basic input</h1>\
  \  <div>\
  \   <form action=\"/\" method=\"POST\">\
  \    <label for=\"name\">Name:</label>\
  \    <input type=\"text\" name=\"name\">\
  \    <label for=\"email\">Email:</label>\
  \    <input type=\"email\" name=\"email\">\
  \    <input type=\"email\" name=\"email\">\
  \    <input type=\"submit\">\
  \   </form>\
  \  <h1>File upload</h1>\
  \  <div>\
  \   <form action=\"/\" method=\"POST\">\
  \    <label for=\"file\">File:</label>\
  \    <input type=\"file\" name=\"file\">\
  \    <input type=\"submit\">\
  \   </form>\
  \  </div>"
  ++"<h1>Request</h1><div>Label:" ++ show lr ++ "</div>"
  ++"<div>" ++ show req ++ "</div>"
  ++"<h1>Document</h1><div>Label:" ++ show ld ++ "</div>"
  ++"<div>" ++ show doc ++ "</div>"
  ++ " </body>\
  \</html>"
