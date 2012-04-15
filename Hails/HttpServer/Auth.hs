{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-| This module exports various authentication methods. -}
module Hails.HttpServer.Auth ( AuthFunction
                             -- * Basic authentication
                             , basicAuth, basicNoAuth
                             -- * External authentication
                             , externalAuth
                             ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Base64
import Data.IterIO.Http
import Data.Functor ((<$>))

import Hails.Crypto

import LIO.DCLabel

type L = L8.ByteString

-- |Authentication function
type AuthFunction m s = HttpReq s -- ^ Request
                      -> m (Either (HttpResp m) (HttpReq s))

--
-- Basic authentication
--

-- | Perform basic authentication
basicAuth :: Monad m
          => (HttpReq s -> m Bool) -- ^ Authentication function
          -> AuthFunction m s
basicAuth authFunc req = 
  case userFromReq of
    Just [user,_] -> do
        success <- authFunc req
        if not success
          then return . Left $ respAuthRequired
          else let hdrs = filter ((/=authField) . fst) $ reqHeaders req
               in return . Right $
                    req { reqHeaders = ("x-hails-user", user) : hdrs }
    _ -> return . Left $ respAuthRequired
  where authField = "authorization"
        -- No login, send an auth response-header:
        respAuthRequired =
         let resp = mkHttpHead stat401
             authHdr = ("WWW-Authenticate", "Basic realm=\"Hails\"")
         in respAddHeader authHdr resp
        -- Get user and password information from request header:
        userFromReq  = let mAuthCode = lookup authField $ reqHeaders req
                       in extractUser . S8.dropWhile (/= ' ') <$> mAuthCode
        extractUser b64u = S8.split ':' $ decodeLenient b64u

-- | Basic authentication, that always succeeds
basicNoAuth :: Monad m => AuthFunction m s
basicNoAuth = basicAuth (const $ return True)

--
-- Cookie authentication
--

-- | Use an external authentication service that sets a cookie.
-- The cookie name is @_hails_user@, and its contents contain
-- a string of the form @user-name:HMAC-SHA1(user-name)@. This
-- function simply checks that the cookie exits and the MAC'd
-- user name is correct. If this is the case, it returns a request
-- with the cookie removed. Otherwise it retuns a redirect (to the
-- provided url) response.
externalAuth :: L -> String -> AuthFunction DC s
externalAuth key url req = 
  let cookies = reqCookies req
      res = do cookie <- lookup _hails_cookie cookies
               (user,mac0) <- doSplit ':' cookie
               let mac1 = showDigest $ hmacSha1 key (lazyfy user)
               if (S8.unpack mac0) == mac1
                 then return $ req { reqCookies =
                         filter ((/= _hails_cookie) . fst) cookies }
                 else Nothing
  in return $ maybe redirect Right res
    where redirect = Left $ resp303 url
          doSplit c xs = case S8.split c xs of
                           [u,m] -> Just (u,m)
                           _     -> Nothing
          lazyfy = L8.pack . S8.unpack
          _hails_cookie = "_hails_user"