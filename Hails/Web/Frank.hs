{- |
Frank is a Sinatra-inspired DSL (see <http://www.sinatrarb.com>) for creating
routes. It is composable with all 'Routeable' types, but is designed to be used
with 'Network.Wai.Controller's. Each verb ('get', 'post', 'put', etc') takes a
URL pattern of the form \"\/dir\/:paramname\/dir\" (see 'routePattern' for
details) and a 'Routeable':

@
module SimpleFrank (server) where
 
import           "Data.String"
import           "Data.Maybe"
import           "Control.Monad"

import           "LIO"
import           "Hails.HttpServer.Types"
import           "Hails.Web"
import qualified "Hails.Web.Frank" as F

server :: 'Application'
server = 'mkRouter' $ do
  F.'get' \"\/users\" $ do
    req \<- 'request' '>>=' unlabel
    return $ 'okHtml' $ 'fromString' $
      \"Welcome Home \" ++ (show $ 'serverName' req)
  F.'get' \"\/users\/:id\" $ do
    userId <- fromMaybe \"\" ``liftM`` 'queryParam' \"id\"
    return $ 'ok' \"text/json\" $ fromString $
      \"{\\\"myid\\\": \" ++ (show userId) ++ \"}\"
  F.'put' \"\/user\/:id\" $ do
  ...
@

With @hails@, you can directly run this:

> hails --app=SimpleFrank

And, with @curl@, you can now checkout your page:

> $ curl localhost:8080/users
> Welcome Home "localhost"
>
> $ curl localhost:8080/users/123
> {"myid": "123"}
>
> $ ...

-}
module Hails.Web.Frank
  ( get
  , post
  , put
  , delete
  , options
  ) where

import           Network.HTTP.Types
import           Hails.Web.Router
import qualified Data.ByteString as S

-- | Helper method
frankMethod :: Routeable r => StdMethod -> S.ByteString -> r -> Route
frankMethod method pattern = routeMethod method . routePattern pattern

-- | Matches the GET method on the given URL pattern
get :: Routeable r => S.ByteString -> r -> Route
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: Routeable r => S.ByteString -> r -> Route
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: Routeable r => S.ByteString -> r -> Route
put = frankMethod PUT

-- | Matches the DELETE method on the given URL pattern
delete :: Routeable r => S.ByteString -> r -> Route
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: Routeable r => S.ByteString -> r -> Route
options = frankMethod OPTIONS
