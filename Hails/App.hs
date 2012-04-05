{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hails.App ( module Hails.IterIO.HailsRoute
                 , module LIO
                 , module LIO.DCLabel
                 , AppReqHandler, AppRoute
                 , getHailsUser, getHailsApp
                 ) where

import Hails.IterIO.HailsRoute
import LIO
import LIO.DCLabel
import LIO.TCB (labelTCB)
import Hails.TCB.Types ( AppReqHandler, AppRoute )
import Data.IterIO.Http.Support.Action (Action, requestHeader)
import qualified Data.ByteString.Char8 as S8


-- | Class for returning user/app information
class GetHailsUser a where
  -- | Get the user the app is running on behalf of
  getHailsUser :: Action t b DC a

-- | Class for returning app/app information
class GetHailsApp a where
  -- | Get the app the app is running on behalf of
  getHailsApp :: Action t b DC a

instance GetHailsUser String where
  getHailsUser = do
    hdr <- requestHeader (S8.pack "x-hails-user")
    maybe (fail "No x-hails-user header") (return . S8.unpack) hdr

instance GetHailsUser (DCLabeled String) where
  getHailsUser = do
    usr <- getHailsUser
    return $ labelTCB (newDC (<>) (usr :: String)) usr

instance GetHailsApp String where
  getHailsApp = do
    hdr <- requestHeader (S8.pack "x-hails-app")
    maybe (fail "No x-hails-app header") (return . S8.unpack) hdr

instance GetHailsApp (DCLabeled String) where
  getHailsApp = do
    usr <- getHailsApp
    return $ labelTCB (newDC (<>) (usr :: String)) usr
