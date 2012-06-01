{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-| This module exports various authentication methods. -}
module Hails.HttpServer.Auth ( Auth, AuthMethod(..)
                             -- * Basic authentication
                             , basicAuth, basicNoAuth
                             -- * External authentication
                             , externalAuth
                             -- * Login action
                             , withUserOrDoAuth 
                             ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Base64
import Data.IterIO.Http
import Data.IterIO.Http.Support (Action, requestHeader, actionResp)
import Data.Maybe
import Data.Functor ((<$>))

import Hails.Crypto

import LIO.DCLabel

import Control.Monad
import Control.Monad.Trans.State (modify)
import Control.Applicative (Applicative(..))
import System.FilePath (takeExtensions)


type L = L8.ByteString

-- | Authentication function. Given a request return an authentication
-- method
type Auth m s = HttpReq s -> AuthMethod m s 

-- | Authentication method consists of an that validates
-- provided credentials in a request, and a computation that given an
-- app response returns an action that inturn returns a response used
-- to present the user with a login \"form\".
data AuthMethod m s = AuthMethod { authValid :: m (HttpReq s)
                                   -- ^ Validator
                                 , authLogin :: HttpResp m -> HttpResp m
                                   -- ^ Login authentication
                                 }

--
-- Basic authentication
--

-- | Perform basic authentication
basicAuth :: (HttpReq s -> DC Bool) -- ^ Authentication function
          -> Auth DC s
basicAuth authFunc req =
  let aV = case userFromReq of
             Just [user,_] -> do
               success <- authFunc req
               let hdrs = filter ((/=authField) . fst) $ reqHeaders req
                   req' = req { reqHeaders =  hdrs }
               return $ if success
                          then maybeAddXHailsUser user req'
                          else req
             _ -> return req
      aL resp = resp `orIfXHailsLogin` respAuthRequired
  in AuthMethod { authValid = aV, authLogin = aL }
  where authField = "authorization"
        -- No login, send an auth response-header:
        respAuthRequired =
         let resp = mkHttpHead stat401
             authHdr = ("WWW-Authenticate", "Basic realm=\"Hails\"")
         in respAddHeader authHdr resp
        -- Get user and password information from request header:
        userFromReq  = let mAuthCode = lookup authField $ reqHeaders req
                       in extractUser . S8.dropWhile (/= ' ') <$> mAuthCode
        --
        extractUser b64u = S8.split ':' $ decodeLenient b64u
        -- add x-hails-user if username is not null
        maybeAddXHailsUser user r =
          if S8.null user
            then r
            else r { reqHeaders = ("x-hails-user", user) : reqHeaders r }

-- | Basic authentication, that always succeeds. 
basicNoAuth :: Auth DC s
basicNoAuth = basicAuth (const $ return True)

--
-- Cookie authentication
--

-- | Use an external authentication service that sets cookies.
-- The cookie names are @_hails_user@, whose contents contains the
-- @user-name@, and @_hails_user_hmac@, whose contents contains
-- @HMAC-SHA1(user-name)@. This function simply checks that the cookie
-- exists and the MAC'd user name is correct. If this is the case, it
-- returns a request with the cookie removed and @x-hails-user@ header
-- set. Otherwies the original request is returned.
-- The login service retuns a redirect (to the provided url).
-- Additionally, cookie @_hails_refer$ is set to the current
-- URL (@scheme://domain:port/path@).
externalAuth :: Show s => L -> String -> Auth DC s
externalAuth key url req =
  let mreqAuth = do
        mac0 <- lookup "_hails_user_hmac" cookies
        user <- lookup "_hails_user" cookies
        let mac1 = showDigest $ hmacSha1 key (lazyfy user)
        if S8.unpack mac0 == mac1
          then return $ req { reqHeaders = ("x-hails-user", user)
                                           : reqHeaders req }
          else Nothing
      aV = return $ fromMaybe req mreqAuth
      aL resp = resp `orIfXHailsLogin` redirectResp
  in AuthMethod { authValid = aV, authLogin = aL }
    where cookies = reqCookies req
          -- redirect to auth service:
          redirectResp = addRedirCookie $ resp303 url
          --
          lazyfy = L8.pack . S8.unpack
          -- "current" url, i.e., referer
          curUrl = S8.unpack $ S8.concat ([ reqScheme', const "://"
                                          , reqHost, const ":", reqPort'
                                          , reqPath] <*> pure req)
          -- add cookie indicating current referer
          addRedirCookie = respAddHeader
                ( S8.pack "Set-Cookie"
                , S8.concat [ "_hails_referer="
                            , S8.pack $ show curUrl
                            , ";path=/;domain="
                            , dropSubDomain (reqHost req)
                            ])
          -- remove subdomain of host:
          -- TODO: make sure this does not leave cookie for .com
          dropSubDomain = S8.pack . takeExtensions . S8.unpack
          -- Request scheme with defaults
          reqScheme' r = let sch  = reqScheme r
                             port = reqPort r
                         in case () of
                             _ | not $ S8.null sch -> sch
                             _ | S8.null sch && port == Just 443 -> "https"
                             _ -> "http"
          -- Request port with defaults
          reqPort' r = let sch  = reqScheme r
                           port = reqPort r
                         in case port of
                              Just p -> S8.pack . show $ p
                              Nothing | sch == "https" -> "443"
                              _ -> "80"


-- | Execute action with user or \"invoke\" authentication service
withUserOrDoAuth :: (String -> Action t b DC ()) -> Action t b DC ()
withUserOrDoAuth act = do
  muser <- getHailsUser
  maybe respond401 act muser
    where getHailsUser = fmap S8.unpack `liftM`
                          requestHeader (S8.pack "x-hails-user")
          resp = respAddHeader ("x-hails-login","yes") $ mkHttpHead stat401
          respond401 = modify $ \s -> s { actionResp = resp }


--
-- Helpers
--

-- | Respond with login response action if \"x-hails-login\" header exists.
orIfXHailsLogin :: Monad m
                => HttpResp m -- ^ App response
                -> HttpResp m -- ^ Response containing the login \"form\"
                -> HttpResp m
orIfXHailsLogin resp0 resp1 =
  if not reqLogin then resp0 else resp1
  where reqLogin = isJust $ lookup "x-hails-login" $ respHeaders resp0
