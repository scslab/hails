{-# LANGUAGE OverloadedStrings #-}

module Hails.HttpServer ( secureHttpServer
												, module Hails.IterIO.HailsRoute
												) where

import Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Server.TCPServer
import DCLabel.TCB
import Hails.IterIO.Conversions
import Hails.IterIO.HailsRoute
import LIO.DCLabel
import LIO.MonadLIO hiding (liftIO)
import LIO.TCB
import Network.Socket as Net

userFromAuthCode :: Maybe S.ByteString -> Maybe String
userFromAuthCode mAuthCode = fmap extractUser mAuthCode
  where extractUser b64u = S.unpack $ S.takeWhile (/= ':') $ decodeLenient b64u

replace :: (a -> Bool) -> a -> [a] -> [a]
replace f a (x:xs) | f x = a:(replace f a xs)
                 | otherwise = x:(replace f a xs)
replace _ _ [] = []

httpApp :: (DCPrivTCB -> HttpRequestHandler DC ()) -> Inum L.ByteString L.ByteString DC ()
httpApp lrh = mkInumM $ do
  req <- httpReqI
  case userFromAuthCode (fmap (S.drop 6) $ Prelude.lookup "authorization" (reqHeaders req)) of
    Just user -> do
      let l = newDC user (<>)
      liftLIO $ lowerClr l
      let pathPrefix = takeWhile (/= '/') $ dropWhile (== '/') $ S.unpack $ reqPath req
      let privilege = createPrivTCB $ newPriv pathPrefix
      let resultReq = req {
        reqHeaders = replace ((== "authorization") . fst) ("authorization", S.pack user) $
                      reqHeaders req
      }
      resp <- liftI $ inumHttpBody req .| lrh privilege resultReq
      resultLabel <- liftLIO $ getLabel
      if resultLabel `leq` l then
        irun $ enumHttpResp resp Nothing
        else
          irun $ enumHttpResp (mkHttpHead stat500) Nothing
    Nothing -> do
      let authRequired = mkHttpHead stat401
      irun $ enumHttpResp (authRequired
        { respHeaders = "WWW-Authenticate: Basic realm=\"Hails\"":(respHeaders authRequired)})
        Nothing

secureHttpServer :: PortNumber -> (DCPrivTCB -> HttpRequestHandler DC ()) -> TCPServer L.ByteString DC
secureHttpServer port lrh = TCPServer port (httpApp lrh) dcServerAcceptor (\m -> fmap fst $ evalDC m)

dcServerAcceptor :: Net.Socket -> DC (Iter L.ByteString DC (), Onum L.ByteString DC ())
dcServerAcceptor sock = do
  (iterIO, onumIO) <- ioTCB $ defaultServerAcceptor sock
  return (iterIOtoIterLIO iterIO, onumIOtoOnumLIO onumIO)
