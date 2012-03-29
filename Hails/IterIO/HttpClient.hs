{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |

Exports basic HTTP client functions inside the 'DC' Monad.
Computations are allowed to communicate over HTTP as long as they can
read and write to a labeled origin. An origin is associated with two
labels. When writing, the origin has a label of the form
@\< {[\"scheme:\/\/authority\"]}, True \>@, where @scheme@ is
either \'http\' or \'https\', and @authority@ is the domain name or IP
address used in the request and port number of the connection. In
other words, the secrecy component contains the origin information,
while the integrity component is the same as that of public data.
When reading, the origin has a label of the form
@\< True, {[\"scheme:\/\/authority\"]} \>@.

This means that 'LIO' (specifically, 'DC') computations can export
data if the current label is not higher than that of the labeled
origin, and read data that is no more trustworthy than that of the
origin.  Practically, this means that untrusted computation can export
data so long as the they have not observed any data more sensitive
than the label of the target domain. Reading (which also occurs on
every request/write) further raises the current label to the join of
the current label and origin.
                                            
For example, suppose some piece of data, @myLoc@, has the label:

> aliceLocL = newDC ("alice" ./\. "http://maps.googleapis.com:80") (<>)

created as:

> myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"


Then, untrusted code (with initial label set to public) running on
behalf of \"alice\" , may perform the following operation:

> let mapBase = "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
> aliceLoc <- urlEncode <$> (unlabelP alicePriv myLoc)
> resp <- simpleGetHttp $ mapBase ++ "&address=" ++ aliceLoc

In this case the 'unlabelP' will raise the current label to the label:

> < {["http://maps.googleapis.com:80"]}, True >

by exercising \"alice\"s privilges.  Directly, the 'simpleHttp'
will be permitted. However, if

> let mapBase = "http://maps.evilalternatives.org/geocode/json?sensor=false"

an exception will be thrown since the current label does not flow to
the label of @mapBase@.
-}

module Hails.IterIO.HttpClient ( -- * Simple interface
                                 HttpRespDC(..)
                               , simpleHttp, simpleHttpP
                               , simpleGetHttp, simpleGetHttpP
                               , simpleHeadHttp, simpleHeadHttpP
                               , extractBody
                               -- * Advanced interface
                               , multiHttp, DCHttpResponseHandler
                               -- * Basic requests
                               , headRequest, getRequest, postRequest
                               )  where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient ( headRequest
                              , getRequest
                              , postRequest
                              , mkHttpClient
                              , httpConnect )
import qualified Data.IterIO.HttpClient as I
import Hails.IterIO.Conversions

import LIO
import LIO.TCB (rtioTCB, getTCB)
import LIO.DCLabel
import LIO.LIORef
import LIO.MonadCatch

import Control.Monad
import Control.Exception (SomeException(..))

import qualified OpenSSL.Session as SSL
import System.Environment

type L = L.ByteString
type S = S.ByteString


-- | Convert an "IterIO" 'HttpResp' to an 'HttpRespDC'
httpRespToDC :: HttpResp IO -> HttpRespDC
httpRespToDC resp =
  HttpRespDC { respStatusDC  = respStatus resp
             , respHeadersDC = respHeaders resp
             , respBodyDC    = getTCB >>= \s -> 
                return $ inumIOtoInumLIO enumHttpBodyResp s }
    where enumHttpBodyResp = respBody resp |. maybeChunk
          maybeChunk = if respChunk resp then inumToChunks else inumNop

--
-- HTTP Response
--

-- | A HTTP response, containing the status, headers, and parsed body.
data HttpRespDC = HttpRespDC { respStatusDC  :: !HttpStatus
                               -- ^ Response status
                             , respHeadersDC :: ![(S, S)]
                               -- ^ Response headers
                             , respBodyDC    :: DC (Onum L DC ())
                               -- ^ Response body
                             } 

-- | Extract body from response
extractBody :: HttpRespDC -> DC L
extractBody resp = do
  bodyOnum <- respBodyDC resp
  l <- getLabel
  ref <- newLIORef l L.empty
  bodyOnum |$ do bdy <- pureI
                 liftLIO $ writeLIORef ref bdy
  readLIORef ref

--
-- Basic functions
--

-- | Perform a simple HTTP request, given the the request header, body
-- and SSL context, if any. Note that that request must have the scheme,
-- host fields set.
simpleHttp :: HttpReq ()   -- ^ Request header
           -> L            -- ^ Request body
           -> DC HttpRespDC
simpleHttp = simpleHttpP noPrivs

-- | Same as 'simpleHttp', but uses privileges.
simpleHttpP :: DCPrivTCB    -- ^ Privilege
            -> HttpReq ()   -- ^ Request header
            -> L            -- ^ Request body
            -> DC HttpRespDC
simpleHttpP p req body = do
  wguardURLP p req
  ctx <- mkSSLContext
  liftM httpRespToDC . rtioTCB $ I.simpleHttp req body ctx

-- 
-- Simple HEAD/GET Wrappers
--

-- | Simple HTTP GET request.
simpleGetHttpP :: DCPrivTCB     -- ^ Privilege
               -> String        -- ^ URL
               -> DC HttpRespDC
simpleGetHttpP p url = simpleHttpP p (getRequest url) L.empty

-- | Simple HTTP GET request.
simpleGetHttp :: String -> DC HttpRespDC
simpleGetHttp = simpleGetHttpP noPrivs

-- | Simple HTTP HEAD request.
simpleHeadHttpP :: DCPrivTCB     -- ^ Privilege
                -> String        -- ^ URL
                -> DC HttpRespDC
simpleHeadHttpP p url = simpleHttpP p (getRequest url) L.empty

-- | Simple HTTP HEAD request.
simpleHeadHttp :: String -> DC HttpRespDC
simpleHeadHttp = simpleHeadHttpP noPrivs


--
-- Labeling URI's
--


-- | Return the labels corresponding to the absolute URI of a request header.
-- The created labels will have the scheme and authority (including port) in the
-- secrecy componenet, and @True@ in the integrity component for the
-- read label (and the dual for write label). Specifically, the
-- labels will have the form:
--
--  > (< {[scheme://authority]}, True >,< True, {[scheme://authority]} >0
--
--  For example, the read label of a request to \"http:\/\/gitstar.com/\" is:
-- 
--  > <{["http://gitstar.com:80"]} , True>
--
--  while the read label of \"https:\/\/gitstar.com:444/\"
--
--  > <{["https://gitstar.com:444"]} , True>
--
-- This should be used for only for single-connection requests, where the
-- absolute URL makes senes.
labelOfReq :: HttpReq () -> Maybe (DCLabel, DCLabel)
labelOfReq req = do
  scheme <- notNull $ reqScheme req
  host   <- notNull $ reqHost req
  port   <- maybe (defaultPort scheme) Just $ reqPort req
  let prin = S8.concat [ scheme
                       , S8.pack "://"
                       , host
                       , S8.pack $ ':' : show port ]
  return (newDC (principal prin) (<>), newDC (<>) (principal prin))
    where defaultPort s | s == S8.pack "http"  = return 80
                        | s == S8.pack "https" = return 443
                        | otherwise = Nothing
          notNull s = if S.null s then Nothing else Just s

--
-- Misc
--

-- | Check that current label can flow to label of request.
wguardURLP :: DCPrivTCB -> HttpReq () -> DC ()
wguardURLP p' req = withCombinedPrivs p' $ \p -> do
  l <- getLabel
  case labelOfReq req of
    Nothing -> throwIO . userError $ "Parse error: cannot create request label"
    Just (lr, lw) -> do
      unless (leqp p l lr) $ throwIO . userError $ 
                "Current label must flow to origin read label"
      taintP p lw

-- | Get the CA directory from environment variable. If set, a 
-- new context is returned.
-- TODO: cache the context.
mkSSLContext :: DC (Maybe SSL.SSLContext)
mkSSLContext = rtioTCB $ do
  env <- getEnvironment
  case lookup "HAILS_SSL_CA_FILE" env of
    Nothing -> return Nothing
    Just caDir -> do ctx <- SSL.context
                     SSL.contextSetCADirectory ctx caDir
                     return $! Just ctx

-- | An HTTP client that reuses a connection to perform multiple
-- requests. Note that a @wguard@ is only performed at the connection
-- establishment.
multiHttp :: (HttpReq (), L)        -- ^ Initial request
          -> DCHttpResponseHandler  -- ^ Request handler
          -> DC ()
multiHttp = multiHttpP noPrivs

-- | Same as 'multiHttp' but uses privileges when performin label
-- comparisons.
multiHttpP :: DCPrivTCB              -- ^ Privilege
           -> (HttpReq (), L)        -- ^ Initial request
           -> DCHttpResponseHandler  -- ^ Request handler
           -> DC ()
multiHttpP p' (req, body) handler = withCombinedPrivs p' $ \p -> do
  let scheme = reqScheme req
      isHttps = scheme == (S8.pack "https")
  port <- maybe (defaultPort scheme) return $ reqPort req
  --
  wguardURLP p req
  ctx <- mkSSLContext
  s <- getTCB
  (sIter,sOnum) <- rtioTCB $ do
    client <- mkHttpClient (reqHost req) port ctx isHttps
    (i,o) <- httpConnect client
    return (iterIOtoIterLIO i, inumIOtoInumLIO o s)
  sOnum |$ dcInumHttpClient (req, body) handler .| sIter
    where defaultPort s | s == S8.pack "http"  = return 80
                        | s == S8.pack "https" = return 443
                        | otherwise = throwIO . userError $
                                        "Unrecognized scheme" ++ S8.unpack s


-- | An HTTP response handler in the 'DC' monad.
type DCHttpResponseHandler = HttpRespDC -> Iter L DC (Maybe (HttpReq (), L)) 

-- | Given an initial request, and a response handler, create an inum
-- that provides underlying functionality of an http client in the 'DC'
-- monad.
dcInumHttpClient :: (HttpReq s, L)
                 -> DCHttpResponseHandler
                 -> Inum L L DC a
dcInumHttpClient (req, body) respHandler = mkInumM $ 
  tryI (irun $ enumHttpReq req body) >>=
             either (fatal . fst) (const loop)
  where loop = do eof <- atEOFI
                  unless eof doresp
        doresp = do
          resp <- liftI $ iterIOtoIterLIO httpRespI
          mreq <- catchI (liftI $ respHandler (httpRespToDC resp)) errH
          maybe (return ())
                (\(req', body') -> do
                  er <- tryI (irun $ enumHttpReq req' body')
                  either (fatal . fst) (const loop) er
                ) mreq
        fatal (SomeException _) = return ()
        errH  (SomeException _) = return . return $ Nothing
  
