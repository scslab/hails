{-# LANGUAGE OverloadedStrings #-}
module SimpleApp (server) where

import           Hails.HttpServer

server :: Application
server _ _ = do
  return $ Response ok200 [] topHtml

topHtml = 
    "<html>\
    \ <head>\
    \  <title>Simple static file server</title>\
    \  <script src=\"/static/js/jquery-1.7.2.min.js\"></script>\
    \  <script src=\"/static/js/lightbox.js\"></script>\
    \  <link href=\"/static/css/lightbox.css\" rel=\"stylesheet\" />\
    \ </head>\
    \ <body>\
    \ Serving images from <a href=\"http://sxc.hu\">stock.xchng</a>:\
    \ <div class=\"imageRow\"> <div class=\"single\">\
    \ <br/>\
    \ <a href=\"/static/images/sf.jpg\" rel=\"lightbox[plants]\" title=\"SF\" class=\"first\">\
    \   <img src=\"/static/images/thumb.sf.jpg\"></a>\
    \ <br/>\
    \ <a href=\"/static/images/nyc.jpg\" rel=\"lightbox[plants]\" title=\"NYC\">\
    \   <img src=\"/static/images/thumb.nyc.jpg\"></a>\
    \ <br/>\
    \ <a href=\"/static/images/paris.jpg\" rel=\"lightbox[plants]\" title=\"Paris\">\
    \   <img src=\"/static/images/thumb.paris.jpg\"></a>\
    \ <br/>\
    \ <a href=\"/static/images/tokyo.jpg\" rel=\"lightbox[plants]\" title=\"Tokyo\" class=\"last\">\
    \   <img src=\"/static/images/thumb.tokyo.jpg\"></a>\
    \ </div></div>\
    \ </body>\
    \</html>"
