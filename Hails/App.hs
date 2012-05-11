{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Hails.App ( module Hails.IterIO.HailsRoute
                 , module LIO
                 , module LIO.DCLabel
                 , AppReqHandler, AppRoute
                 -- * Info about app and user
                 , getHailsUser, haveHailsUser
                 , withUserOrRedirectToAuth 
                 , getHailsApp
                 ) where

import Hails.HttpServer.Auth (withUserOrRedirectToAuth)
import Hails.IterIO.HailsRoute
import LIO
import LIO.DCLabel
import Hails.TCB.Types ( AppReqHandler, AppRoute )
import Data.IterIO.Http.Support.Action (Action, requestHeader)
import qualified Data.ByteString.Char8 as S8

import Data.Maybe (isJust)
import Control.Monad (liftM)

-- | Get the user the app is running on behalf of
getHailsUser :: Action t b DC (Maybe String)
getHailsUser = fmap S8.unpack `liftM` requestHeader (S8.pack "x-hails-user")

-- | App is running on behalf of a user
haveHailsUser :: Action t b DC Bool
haveHailsUser = isJust `liftM` getHailsUser

-- | Get the app the app is running on behalf of
getHailsApp :: Action t b DC String
getHailsApp = do
  hdr <- requestHeader (S8.pack "x-hails-app")
  maybe (fail "No x-hails-app header") (return . S8.unpack) hdr
