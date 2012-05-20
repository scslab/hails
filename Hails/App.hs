{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Hails.App ( module Hails.IterIO.HailsRoute
                 , module LIO
                 , module LIO.DCLabel
                 , AppReqHandler, AppRoute
                 -- * Info about app and user
                 , getHailsUser, getHailsApp
                 ) where

import Hails.IterIO.HailsRoute
import LIO
import LIO.DCLabel
import Hails.TCB.Types ( AppReqHandler, AppRoute )
import Data.IterIO.Http.Support.Action (Action, requestHeader)
import qualified Data.ByteString.Char8 as S8

-- | Get the user the app is running on behalf of
getHailsUser :: Action t b DC (Maybe String)
getHailsUser = do
  hdr <- requestHeader (S8.pack "x-hails-user")
  return $ fmap S8.unpack hdr

-- | Get the app the app is running on behalf of
getHailsApp :: Action t b DC String
getHailsApp = do
  hdr <- requestHeader (S8.pack "x-hails-app")
  maybe (fail "No x-hails-app header") (return . S8.unpack) hdr
