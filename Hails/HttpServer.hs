{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

{-# LANGUAGE OverloadedStrings #-}

module Hails.HttpServer ( secureHttpServer ) where

import Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Server.TCPServer
import Data.Functor ((<$>))

import Hails.TCB.Types

import Hails.IterIO.Conversions
import DCLabel.TCB
import LIO.DCLabel
import LIO.MonadLIO hiding (liftIO)
import LIO.TCB
import Network.Socket as Net


lpub :: DCLabel
lpub = newDC (<>) (<>)

-- | Given an 'App' return handler.
httpApp :: AppReqHandler -> Inum L.ByteString L.ByteString DC ()
httpApp lrh = mkInumM $ do
  req0 <- httpReqI
  appState <- getAppConf req0
  case appState of
    Left resp -> irun $ enumHttpResp resp Nothing
    Right appC -> do
      let userLabel = newDC (appUser appC) (<>)
          req = appReq appC
      -- Set current label to be public, clearance to the user's label
      -- and privilege to the app's privilege.
      liftLIO $ do taint lpub 
                   lowerClr userLabel
                   setPrivileges (appPriv appC)
      -- TODO: catch exceptions
      resp <- liftI $ inumHttpBody req .| lrh req
      resultLabel <- liftLIO $ getLabel
      irun $ (flip enumHttpResp) Nothing $ 
        if resultLabel `leq` userLabel
          then resp 
          else resp500 "App violated IFC" --TODO: add custom header

-- | Return a server, given a port number and app.
secureHttpServer :: PortNumber -> AppReqHandler -> TCPServer L.ByteString DC
secureHttpServer port lrh = TCPServer port (httpApp lrh) dcServerAcceptor
  (\m -> fmap fst $ evalDC m)

-- | Given a socket, return the to/from-browser pipes.
dcServerAcceptor :: Net.Socket -> DC (Iter L.ByteString DC (), Onum L.ByteString DC ())
dcServerAcceptor sock = do
  (iterIO, onumIO) <- ioTCB $ defaultServerAcceptor sock
  return (iterIOtoIterLIO iterIO, onumIOtoOnumLIO onumIO)

--
-- Helper
--

-- | Get the authenticated user, application name, and new (safe)
-- request.
getAppConf :: (Monad m, Monad m')
            => HttpReq ()
            -> m (Either (HttpResp m') AppConf)
getAppConf req0 = do
  authRes <- tryAuthUser req0
  case authRes of
    Left resp -> return . Left $ resp
    Right (user, req) ->
      let u = principal $ user
          n = C.unpack $ C.takeWhile (/= '.') $ reqHost req
          p = createPrivTCB $ newPriv n
      in return . Right $ AppConf { appUser = u
                                  , appName = n
                                  , appPriv = p
                                  , appReq  = req }


-- | Get the authenticated user information and remove and sensitive
-- headers from request.
tryAuthUser :: (Monad m, Monad m')
            => HttpReq s
            -> m (Either (HttpResp m') (C.ByteString, HttpReq s))
tryAuthUser req = do
  case userFromReq of
    Nothing -> return . Left $ respAuthRequired
    Just user -> do
        return . Right $
          ( user, req {
              reqHeaders = filter ((/=authField) . fst) $ reqHeaders req
                      })
  where authField = "authorization"
        -- No login, send an auth response-header:
        respAuthRequired =
         let hdr = mkHttpHead stat401
             authHdr = "WWW-Authenticate: Basic realm=\"Hails\""
         in hdr { respHeaders = authHdr : (respHeaders hdr) }
        -- Get user information from request header:
        userFromReq  = let mAuthCode = lookup authField $ reqHeaders req
                       in extractUser . (C.dropWhile (/= ' ')) <$> mAuthCode
        extractUser b64u = C.takeWhile (/= ':') $ decodeLenient b64u

