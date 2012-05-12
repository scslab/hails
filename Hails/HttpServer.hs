{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

{-# LANGUAGE OverloadedStrings #-}

module Hails.HttpServer ( secureHttpServer ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.Monoid (mempty)
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute (runHttpRoute, routeName)
import Data.IterIO.Server.TCPServer
import Data.Functor ((<$>))

import Hails.TCB.Types

import Hails.IterIO.Mime
import Hails.IterIO.Conversions
import Hails.IterIO.HailsRoute
import DCLabel.TCB
import LIO.DCLabel
import LIO.MonadLIO hiding (liftIO)
import LIO.TCB
import Network.Socket as Net

import Hails.HttpServer.Auth

import System.Environment

type L = L8.ByteString

-- | Given an 'App' return handler.
httpApp :: AuthFunction DC () -> AppReqHandler -> Inum L L DC ()
httpApp authFunc appHndl = mkInumM $ do
  req0 <- httpReqI
  authRes <- liftLIO $ authFunc req0
  case authRes of
    Left resp  -> irun $ enumHttpResp resp
    Right req1 -> irun . enumHttpResp =<< do
      let appConf = getAppConf req1
          req     = appReq appConf
      if isStaticReq req
        then liftI $ respondWithFS req
        else do body <- inumHttpBody req .| pureI
                let browserLabel = appBrowserLabel appConf
                    ap           = appPriv appConf
                -- Set current label to be public, clearance to the user's label
                -- if looged in (otherwise public) and privilege to the app's
                -- privilege.
                (resp, resultLabel) <- liftLIO $ do
                   lowerClr $ browserLabel
                   setLabelTCB lpub
                   setPrivileges ap
                   -- TODO: catch exceptions:
                   respRaw <- appHndl req $ labelTCB (appReqLabel appConf) body
                   resultLabel <- (\lg -> lostar ap lg lpub) <$> getLabel
                   -- function to add x-hails-sensitive header:
                   let respF = if resultLabel `leq` lpub
                                then id
                                else addXHailsSensitive
                   return (respF respRaw, resultLabel)
                return $ if resultLabel `leq` browserLabel
                           then resp
                           else resp403 req -- User can't see (TODO: loop forever)
  where isStaticReq req | null . reqPathLst $ req = False
                        | otherwise = (head . reqPathLst $ req) == "static"
        -- if /static, respond by routing files from filesystem
        respondWithFS req = 
          let rh = runHttpRoute $ routeName "static" $
                    routeFileSys systemMimeMap (const mempty) "static"
          in inumHttpBody req .| rh req
        -- add x-hails-sensitive header
        addXHailsSensitive = respAddHeader ("x-hails-sensitive", "Yes")

-- | Return a server, given a port number and app.
secureHttpServer :: AuthFunction DC ()
                 -> PortNumber
                 -> AppReqHandler
                 -> TCPServer L DC
secureHttpServer authFunc port appHandler =
  TCPServer port app dcServerAcceptor evalDCWithFS
  where app = httpApp authFunc appHandler

-- | Run 'DC' action, with filesystem, if filesystem root is supplied.
evalDCWithFS :: DC a -> IO a
evalDCWithFS act = do
  env <- getEnvironment
  let eval = maybe evalDC
                   (\fp -> evalDCWithRoot fp (Just lpub)) $
                   lookup "HAILS_FS_ROOT" env
  fst <$> eval act


-- | Given a socket, return the to/from-browser pipes.
dcServerAcceptor :: Net.Socket -> DC (Iter L DC (), Onum L DC ())
dcServerAcceptor sock = do
  (iterIO, onumIO) <- ioTCB $ defaultServerAcceptor sock
  s <- getTCB
  return (iterIOtoIterLIO iterIO, inumIOtoInumLIO onumIO s)

--
-- Helper
--

-- | Get the browser label, application name, and new (safe)
-- request. Note: all cookies are removed.
getAppConf :: HttpReq () -> AppConf
getAppConf req =
  let hdrs = reqHeaders req
      usrN  = principal `fmap` lookup "x-hails-user" hdrs
      appN  = S8.unpack . S8.takeWhile (/= '.') $ reqHost req
      privs = createPrivTCB $ newPriv appN
  in AppConf { appBrowserLabel = maybe lpub (\u -> newDC u (<>)) usrN
             , appName         = appN
             , appPriv         = privs
             , appReq          = modReq appN hdrs
             , appReqLabel     = maybe lpub (\u -> newDC (<>) u) usrN
             }
    where modReq n hdrs =
            req { reqHeaders = ("x-hails-app", S8.pack n) : hdrs
                , reqCookies = [] }
