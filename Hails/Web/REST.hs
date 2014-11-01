{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{- |
REST is a DSL for creating routes using RESTful HTTP verbs.
See <http://en.wikipedia.org/wiki/Representational_state_transfer>

For example, an app handling users may define a REST controller as:

@
module SimpleREST (server) where
 
import           "Data.String"
import           "Data.Maybe"
import           "Control.Monad"

import           "LIO"
import           "Hails.HttpServer.Types"
import           "Hails.Web"
import qualified "Hails.Web.REST" as REST

server :: 'Application'
server = 'mkRouter' $ 'routeName' \"users\" $ do
  REST.'index' $ do
    req \<- 'request' '>>=' unlabel
    return $ 'okHtml' $ 'fromString' $
      \"Welcome Home \" ++ (show $ 'serverName' req)
  REST.'show' $ do
    userId <- fromMaybe \"\" ``liftM`` 'queryParam' \"id\"
    return $ 'ok' \"text/json\" $ fromString $
      \"{\\\"myid\\\": \" ++ (show userId) ++ \"}\"
  ...
@

With @hails@, you can directly run this:

> hails --app=SimpleREST

And, with @curl@, you can now checkout your page:

> $ curl localhost:8080/users
> Welcome Home "localhost"
>
> $ curl localhost:8080/users/123
> {"myid": "123"}
>
> $ ...


-}
module Hails.Web.REST
  ( RESTController
  , index, show, create, update, delete
  , edit, new
  ) where

import Prelude hiding (show, pi)

import LIO.DCLabel

import Control.Monad.Trans.State
import Hails.Web.Responses
import Hails.Web.Router
import Network.HTTP.Types

-- | Type used to encode a REST controller.
data RESTControllerState = RESTControllerState
  { restIndex   :: Route
  , restShow    :: Route
  , restCreate  :: Route
  , restUpdate  :: Route
  , restDelete  :: Route
  , restEdit    :: Route
  , restNew     :: Route
  }

-- | Default state, returns @404@ for all verbs.
defaultRESTControllerState :: RESTControllerState
defaultRESTControllerState = RESTControllerState
  { restIndex   = routeAll $ notFound
  , restShow    = routeAll $ notFound
  , restCreate  = routeAll $ notFound
  , restUpdate  = routeAll $ notFound
  , restDelete  = routeAll $ notFound
  , restEdit    = routeAll $ notFound
  , restNew     = routeAll $ notFound
  }

instance Routeable RESTControllerState where
  runRoute controller = runRoute $ do
    routeMethod GET $ do
      routeTop $ restIndex controller
      routeName "new" $ restNew controller
      routeVar "id" $ do
        routeTop $ restShow controller
        routeName "edit" $ restEdit controller

    routeMethod POST $ routeTop $ restCreate controller

    routeMethod DELETE $ routeVar "id" $ restDelete controller

    routeMethod PUT $ routeVar "id" $ restUpdate controller

-- | Monad used to encode a REST controller incrementally.
type RESTControllerM a = StateT RESTControllerState DC a

-- | Monad used to encode a REST controller incrementally.
-- The return type is not used, hence always '()'.
type RESTController = RESTControllerM ()

instance Routeable (RESTControllerM a) where
  runRoute controller = rt
    where rt pi eq conf req = do
            (_, st) <- runStateT controller defaultRESTControllerState
            runRoute st pi eq conf req


-- |GET \/
index :: Routeable r => r -> RESTController
index route = modify $ \controller ->
  controller { restIndex = routeAll route }

-- |POST \/
create :: Routeable r => r -> RESTController
create route = modify $ \controller ->
  controller { restCreate = routeAll route }

-- |GET \/:id\/edit
edit :: Routeable r => r -> RESTController
edit route = modify $ \controller ->
  controller { restEdit = routeAll route }

-- |GET \/new
new :: Routeable r => r -> RESTController
new route = modify $ \controller ->
  controller { restNew = routeAll route }

-- |GET \/:id
show :: Routeable r => r -> RESTController
show route = modify $ \controller ->
  controller { restShow = routeAll route }

-- |PUT \/:id
update :: Routeable r => r -> RESTController
update route = modify $ \controller ->
  controller { restUpdate = routeAll route }

-- |DELETE \/:id
delete :: Routeable r => r -> RESTController
delete route = modify $ \controller ->
  controller { restDelete = routeAll route }

