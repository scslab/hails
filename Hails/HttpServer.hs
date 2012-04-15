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

import Hails.IterIO.Conversions
import Hails.IterIO.HailsRoute
import DCLabel.TCB
import LIO.DCLabel
import LIO.MonadLIO hiding (liftIO)
import LIO.TCB
import Network.Socket as Net

import Hails.HttpServer.Auth

type L = L8.ByteString

-- | Given an 'App' return handler.
httpApp :: AuthFunction DC () -> AppReqHandler -> Inum L L DC ()
httpApp authFunc lrh = mkInumM $ do
  req0 <- httpReqI
  authRes <- liftLIO $ authFunc req0
  case authRes of
    Left resp  -> irun $ enumHttpResp resp
    Right req1 -> do
      appState <- getAppConf req1
      irun . enumHttpResp =<<
        case appState of
         Nothing -> return $ resp500 "Missing x-hails-user header"
         Just appC | isStaticReq appC -> liftI $ respondWithFS (appReq appC)
                   | otherwise -> do
           let userLabel = newDC (appUser appC) (<>)
               req = appReq appC
           -- Set current label to be public, clearance to the user's label
           -- and privilege to the app's privilege.
           liftLIO $ do taint lpub
                        lowerClr userLabel
                        setPrivileges (appPriv appC)
           body <- inumHttpBody req .| pureI
           -- TODO: catch exceptions:
           resp <- liftLIO $ lrh req (labelTCB (newDC (<>) (appUser appC)) body)
           resultLabel <- liftLIO getLabel
           return $ if resultLabel `leq` userLabel
                      then resp
                      else resp500 "App violated IFC"
  where isStaticReq appC | null . reqPathLst . appReq $ appC = False
                         | otherwise =
                            (head . reqPathLst . appReq $ appC) == "static"
        -- if /static, respond by routing files from filesystem
        respondWithFS req = 
          let rh = runHttpRoute $ routeName "static" $
                    routeFileSys systemMimeMap (const mempty) "static"
          in inumHttpBody req .| rh req

-- | Return a server, given a port number and app.
secureHttpServer :: AuthFunction DC ()
                 -> PortNumber
                 -> AppReqHandler
                 -> TCPServer L DC
secureHttpServer authFunc port appHandler =
  TCPServer port app dcServerAcceptor handler
  where handler m = fst <$> evalDC m
        app = httpApp authFunc appHandler

-- | Given a socket, return the to/from-browser pipes.
dcServerAcceptor :: Net.Socket -> DC (Iter L DC (), Onum L DC ())
dcServerAcceptor sock = do
  (iterIO, onumIO) <- ioTCB $ defaultServerAcceptor sock
  s <- getTCB
  return (iterIOtoIterLIO iterIO, inumIOtoInumLIO onumIO s)

--
-- Helper
--

-- | Get the authenticated user, application name, and new (safe)
-- request. Note: all cookies are removed.
getAppConf :: (Monad m)
            => HttpReq ()
            -> m (Maybe AppConf)
getAppConf req =
  let hdrs = reqHeaders req
  in case lookup "x-hails-user" hdrs of
       Nothing -> return Nothing
       Just user ->
         let usrN  = principal user
             appN  = S8.unpack . S8.takeWhile (/= '.') $ reqHost req
             privs = createPrivTCB $ newPriv appN
         in return . Just $ AppConf { appUser = usrN
                                    , appName = appN
                                    , appPriv = privs
                                    , appReq  = modReq appN hdrs}
    where modReq n hdrs =
            req { reqHeaders = ("x-hails-app", S8.pack n) : hdrs
                , reqCookies = [] }
