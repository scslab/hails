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

import Control.Applicative (Applicative(..))
import System.FilePath (takeExtensions)

type S = S8.ByteString
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

-- | Basic authentication, that always succeeds. The function uses the
-- username in the cookie (as in 'externalAuth'), if it is set. If the
-- cookie is not set, 'bsicAuth' is used.
basicNoAuth :: Monad m => AuthFunction m s
basicNoAuth req =
  let cookies = reqCookies req
  in case lookup _hails_cookie cookies of
    Just user -> return . Right $ req { reqCookies =
                        filter (not . S8.isPrefixOf _hails_cookie . fst) cookies
                      , reqHeaders = ("x-hails-user", user) : reqHeaders req }
    Nothing -> basicAuth (const $ return True) req

--
-- Cookie authentication
--

-- | Use an external authentication service that sets a cookie.
-- The cookie name is @_hails_user@, and its contents contain
-- a string of the form @user-name:HMAC-SHA1(user-name)@. This
-- function simply checks that the cookie exits and the MAC'd
-- user name is correct. If this is the case, it returns a request
-- with the cookie removed. Otherwise it retuns a redirect (to the
-- provided url) response. Before redirecting a cookie @_hails_refer$
-- is set to the current @scheme://
externalAuth :: L -> String -> AuthFunction DC s
externalAuth key url req = 
  let cookies = reqCookies req
      res = do user <- lookup _hails_cookie cookies
               mac0 <- lookup _hails_cookie_hmac cookies
               let mac1 = showDigest $ hmacSha1 key (lazyfy user)
               if S8.unpack mac0 == mac1
                 then return $ req { reqCookies =
                        filter (not . S8.isPrefixOf _hails_cookie . fst) cookies
                      , reqHeaders = ("x-hails-user", user) : reqHeaders req }
                 else Nothing
  in return $ maybe redirect Right res
    where redirect = Left $ addRedirCookie $ resp303 url
          lazyfy = L8.pack . S8.unpack
          curUrl = S8.unpack $ S8.concat ([ reqScheme', const "://"
                                          , reqHost, const ":", reqPort'
                                          , reqPath] <*> pure req)
          addRedirCookie = respAddHeader
                ( S8.pack "Set-Cookie"
                , S8.concat [ _hails_cookie_referer
                            , "="
                            , S8.pack $ show curUrl
                            , ";path=/;domain="
                            , dropSubDomain (reqHost req)
                            ])
          dropSubDomain = S8.pack . takeExtensions . S8.unpack
          reqScheme' r= let sch  = reqScheme r
                            port = reqPort r
                        in case () of
                            _ | not $ S8.null sch -> sch
                            _ | S8.null sch && port == Just 443 -> "https"
                            _ -> "http"
          reqPort' r= let sch  = reqScheme r
                          port = reqPort r
                        in case port of
                             Just p -> S8.pack . show $ p
                             Nothing | sch == "https" -> "443"
                             _ -> "80"

-- | Cookie user token
_hails_cookie :: S
_hails_cookie = "_hails_user"

-- | Cookie user HMAC token
_hails_cookie_hmac :: S
_hails_cookie_hmac = "_hails_user_hmac"

-- | Referer needed by authentication service
_hails_cookie_referer :: S
_hails_cookie_referer = "_hails_referer"
