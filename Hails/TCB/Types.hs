{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE DeriveFunctor,
             GeneralizedNewtypeDeriving #-}
             
module Hails.TCB.Types ( AppName
                       , AppConf(..)
                       , AppReqHandler
                       , AppRoute
                       , AppSessionData(..)
                       ) where

import qualified Data.ByteString.Lazy as L
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute

import DCLabel.TCB
import LIO.DCLabel

type L = L.ByteString

-- | Application name
type AppName = String

-- | Application configuration.
data AppConf = AppConf { appUser :: !Principal
                         -- ^ User the app is running on behalf of
                         , appName :: !AppName
                         -- ^ The app's name
                         , appPriv :: !TCBPriv
                         -- ^ The app's privileges.
                         , appReq  :: HttpReq (AppSessionData DCLabel)
                         -- ^ The request message
                         }

-- | Application handler.
type AppReqHandler = HttpReq (AppSessionData DCLabel)
                   -> Iter L DC (HttpResp DC)

-- | Session data to be passed to apps in an 'HttpReq'. The
-- constructor must not be exported to untrusted code.
data AppSessionData l = AppSessionDataTCB l

-- | Application route.
type AppRoute = HttpRoute DC ()
