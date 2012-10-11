{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
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

data RESTControllerState = RESTControllerState
  { restIndex   :: Route ()
  , restShow    :: Route ()
  , restCreate  :: Route ()
  , restUpdate  :: Route ()
  , restDelete  :: Route ()
  , restEdit    :: Route ()
  , restNew     :: Route ()
  }

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

    routeMethod PUT $ restUpdate controller

    routeMethod DELETE $ restDelete controller

type RESTController a = StateT RESTControllerState DC a

instance Routeable (RESTController a) where
  runRoute controller = rt
    where rt pi conf req = do
            (_, st) <- runStateT controller defaultRESTControllerState
            runRoute st pi conf req

index :: Routeable r => r -> RESTController ()
index route = modify $ \controller ->
  controller { restIndex = routeAll route }

create :: Routeable r => r -> RESTController ()
create route = modify $ \controller ->
  controller { restCreate = routeAll route }

edit :: Routeable r => r -> RESTController ()
edit route = modify $ \controller ->
  controller { restEdit = routeAll route }

new :: Routeable r => r -> RESTController ()
new route = modify $ \controller ->
  controller { restNew = routeAll route }

show :: Routeable r => r -> RESTController ()
show route = modify $ \controller ->
  controller { restShow = routeAll route }

update :: Routeable r => r -> RESTController ()
update route = modify $ \controller ->
  controller { restUpdate = routeAll route }

delete :: Routeable r => r -> RESTController ()
delete route = modify $ \controller ->
  controller { restDelete = routeAll route }

