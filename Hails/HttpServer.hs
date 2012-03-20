{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

{-# LANGUAGE OverloadedStrings #-}

module Hails.HttpServer ( secureHttpServer ) where

import Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8

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


type L = L8.ByteString
type S = S8.ByteString

-- | Given an 'App' return handler.
httpApp :: AppReqHandler -> Inum L L DC ()
httpApp lrh = mkInumM $ do
  req0 <- httpReqI
  appState <- getAppConf req0
  case appState of
    Left resp -> irun $ enumHttpResp resp
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
      irun $ enumHttpResp $ 
        if resultLabel `leq` userLabel
          then resp 
          else resp500 "App violated IFC" --TODO: add custom header

-- | Return a server, given a port number and app.
secureHttpServer :: PortNumber -> AppReqHandler -> TCPServer L DC
secureHttpServer port lrh = TCPServer port (httpApp lrh) dcServerAcceptor
  (\m -> fmap fst $ evalDC m)

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
-- request.
getAppConf :: (Monad m, Monad m')
            => HttpReq ()
            -> m (Either (HttpResp m') AppConf)
getAppConf req0 = do
  authRes <- tryAuthUser req0
  case authRes of
    Left resp -> return . Left $ resp
    Right (user, req) ->
      let usrN = principal $ user
          appN = S8.unpack . S8.takeWhile (/= '.') $ reqHost req
          priv = createPrivTCB $ newPriv appN
      in return . Right $ AppConf { appUser = usrN
                                  , appName = appN
                                  , appPriv = priv
                                  , appReq  = addAppHdr req appN }
    where addAppHdr req n = 
            req { reqHeaders = ("x-hails-app", S8.pack n) : reqHeaders req }


-- | Get the authenticated user information and remove and sensitive
-- headers from request.
tryAuthUser :: (Monad m, Monad m')
            => HttpReq s
            -> m (Either (HttpResp m') (S, HttpReq s))
tryAuthUser req = do
  case userFromReq of
    Nothing -> return . Left $ respAuthRequired
    Just user -> do
        return . Right $
          let hdrs = filter ((/=authField) . fst) $ reqHeaders req
          in (user, req { reqHeaders = ("x-hails-user", user) : hdrs })
  where authField = "authorization"
        -- No login, send an auth response-header:
        respAuthRequired =
         let resp = mkHttpHead stat401
             authHdr = ("WWW-Authenticate", "Basic realm=\"Hails\"")
         in respAddHeader authHdr resp
        -- Get user information from request header:
        userFromReq  = let mAuthCode = lookup authField $ reqHeaders req
                       in extractUser . (S8.dropWhile (/= ' ')) <$> mAuthCode
        extractUser b64u = S8.takeWhile (/= ':') $ decodeLenient b64u

