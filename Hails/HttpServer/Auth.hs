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
                             -- * Login action
                             , withUserOrRedirectToAuth 
                             ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Base64
import Data.IterIO.Http
import Data.IterIO.Http.Support (Action, requestHeader, redirectTo, actionResp)
import Data.Functor ((<$>))
import Data.IORef

import Hails.Crypto

import LIO
import LIO.DCLabel
import LIO.TCB (ioTCB)

import Control.Monad
import Control.Monad.Trans.State (modify)
import Control.Applicative (Applicative(..))
import System.FilePath (takeExtensions)
import System.IO.Unsafe

type S = S8.ByteString
type L = L8.ByteString

-- |Authentication function
type AuthFunction m s = HttpReq s -- ^ Request
                      -> m (Either (HttpResp m) (HttpReq s))

--
-- Basic authentication
--

-- | Perform basic authentication
basicAuth :: (HttpReq s -> DC Bool) -- ^ Authentication function
          -> AuthFunction DC s
basicAuth authFunc req = do
  setLoginAction $ respondWithResp respAuthRequired
  case userFromReq of
    Just [user,_] -> do
        success <- authFunc req
        if not success
          then return . Left $ respAuthRequired
          else let hdrs = filter ((/=authField) . fst) $ reqHeaders req
                   req' = req { reqHeaders =  hdrs }
               in return . Right $ maybeAddXHailsUser user req'
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
-- cookie is not set, 'basicAuth' is used.
basicNoAuth :: AuthFunction DC s
basicNoAuth req =
  let cookies     = reqCookies req
      f           = not . S8.isPrefixOf _hails_cookie . fst
      reqNoCookie = req { reqCookies = filter f cookies }
  in case lookup _hails_cookie cookies of
    Just user -> return . Right $ maybeAddXHailsUser user reqNoCookie
    Nothing   -> basicAuth (const $ return True) req

-- | Add @x-hails-user@ header if username is not blank. No username
-- is treated as public.
maybeAddXHailsUser :: S -> HttpReq s -> HttpReq s
maybeAddXHailsUser user req =
  if S8.null user
    then req
    else req { reqHeaders = ("x-hails-user", user) : reqHeaders req }

--
-- Cookie authentication
--

-- | Use an external authentication service that sets cookies.
-- The cookie names are @_hails_user@, whose contents contains the
-- @user-name@, and @_hails_user_hmac@, whose contents contains
-- @HMAC-SHA1(user-name)@. This function simply checks that the cookie
-- exits and the MAC'd user name is correct. If this is the case, it
-- returns a request with the cookie removed and @x-hails-user@ header
-- set. Otherwise it retuns a redirect (to the provided url) response.
-- Before redirecting a cookie @_hails_refer$ is set to the current
-- URL (@scheme://domain:port/path@).
externalAuth :: L -> String -> AuthFunction DC s
externalAuth key url req = do
  setLoginAction $ redirectTo url
  let cookies = reqCookies req
      res = do user <- lookup _hails_cookie cookies
               mac0 <- lookup _hails_cookie_hmac cookies
               let mac1 = showDigest $ hmacSha1 key (lazyfy user)
               if S8.unpack mac0 == mac1
                 then return $ req { reqCookies =
                        filter (not . S8.isPrefixOf _hails_cookie . fst) cookies
                      , reqHeaders = ("x-hails-user", user) : reqHeaders req }
                 else Nothing
  return $ maybe redirect Right res
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
          reqScheme' r = let sch  = reqScheme r
                             port = reqPort r
                         in case () of
                             _ | not $ S8.null sch -> sch
                             _ | S8.null sch && port == Just 443 -> "https"
                             _ -> "http"
          reqPort' r = let sch  = reqScheme r
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

--
-- Handle login
--

-- TODO: This  is an ugly hack and should be encoded in the Action
-- monad in the next release

-- | What do do if user is not logged-in with 'withUserOrRedirectToAuth'
loginActionRef :: IORef (Action t b DC ())
{-# NOINLINE loginActionRef #-}
loginActionRef = unsafePerformIO $ newIORef (return ())

-- | Set the login action
setLoginAction :: Action t b DC () -> DC ()
setLoginAction act = ioTCB $ writeIORef loginActionRef act

-- | Get the login action
getLoginAction :: DC (Action t b DC ())
getLoginAction = ioTCB $ readIORef loginActionRef
  
-- | Respond explicitly
respondWithResp :: HttpResp DC -> Action t b DC ()
respondWithResp resp = modify $ \s -> s { actionResp = resp }

-- | Execute action with user or redirect to authentication service
withUserOrRedirectToAuth :: (String -> Action t b DC ()) -> Action t b DC ()
withUserOrRedirectToAuth act = do
  muser <- getHailsUser
  loginAction <- liftLIO getLoginAction
  maybe loginAction act muser
    where getHailsUser = fmap S8.unpack `liftM` requestHeader (S8.pack "x-hails-user")


