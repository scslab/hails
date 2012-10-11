{- |
Frank is a Sinatra-inspired DSL (see <http://www.sinatrarb.com>) for creating
routes. It is composable with all 'Routeable' types, but is designed to be used
with 'Network.Wai.Controller's. Each verb ('get', 'post', 'put', etc') takes a
URL pattern of the form \"\/dir\/:paramname\/dir\" (see 'routePattern' for
details) and a 'Routeable':

@
  main :: IO ()
  main = runSettings defaultSettings $ mkRouter $ do
    get \"\/\" $ do
      req <- request
      return $ okHtml $ fromString $
        \"Welcome Home \" ++ (show $ serverName req)
    get \"\/user\/:id\" $ do
      userId \<- queryParam \"id\" >>= fromMaybe \"\"
      return $ ok \"text/json\" $ fromString $
        \"{\\\"myid\\\": \" ++ (show userId) ++ \"}\"
    put \"\/user\/:id\" $ do
      ...
@

-}
module Hails.Web.Frank
  ( get
  , post
  , put
  , delete
  , options
  ) where

import Network.HTTP.Types
import Hails.Web.Router
import qualified Data.ByteString as S

-- | Helper method
frankMethod :: Routeable r => StdMethod -> S.ByteString -> r -> Route ()
frankMethod method pattern = routeMethod method . routePattern pattern

-- | Matches the GET method on the given URL pattern
get :: Routeable r => S.ByteString -> r -> Route ()
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: Routeable r => S.ByteString -> r -> Route ()
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: Routeable r => S.ByteString -> r -> Route ()
put = frankMethod PUT

-- | Matches the DELETE method on the given URL pattern
delete :: Routeable r => S.ByteString -> r -> Route ()
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: Routeable r => S.ByteString -> r -> Route ()
options = frankMethod OPTIONS

