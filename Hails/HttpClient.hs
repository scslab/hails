{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts #-}
{- |

Exports basic HTTP client functions inside the 'DC' Monad.
Computations are allowed to communicate over HTTP as long as they can
read and write to a labeled origin. An origin is associated with two
labels. When writing, the origin has a label of the form
@\< \"scheme:\/\/authority\", |True \>@, where @scheme@ is
either \'http\' or \'https\', and @authority@ is the domain name or IP
address used in the request and port number of the connection. In
other words, the secrecy component contains the origin information,
while the integrity component is the same as that of public data.
When reading, the origin has a label of the form
@\< |True, \"scheme:\/\/authority\" \>@.

This means that 'DC' computations can export data if the current label
is not higher than that of the labeled origin, and read data that is
no more trustworthy than that of the origin.  Practically, this means
that untrusted computation can export data so long as the they have
not observed any data more sensitive than the label of the target
domain. Reading (which also occurs on every request/write) further
raises the current label to the join of the current label and origin.
                                            
For example, suppose some piece of data, @myLoc@, has the label:

> aliceLocL = dcLabel ("alice" /\ "http://maps.googleapis.com:80") dcTrue

created as:

> myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"


Then, untrusted code (with initial label set to public) running on
behalf of \"alice\" , may perform the following operation:

> let mapBase = "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
> aliceLoc <- unlabelP alicePriv myLoc
> resp <- simpleGetHttp $ mapBase ++ "&address=" ++ aliceLoc

In this case the 'unlabelP' will raise the current label to the label:

> < "http://maps.googleapis.com:80", |True >

by exercising \"alice\"s privilges.  Directly, the 'simpleHttp'
will be permitted. However, if

> let mapBase = "http://maps.evilalternatives.org/geocode/json?sensor=false"

an exception will be thrown since the current label does not flow to
the label of @mapBase@.



This module uses 'http-conduit' as the underlying client, we recommend
looking at the "Network.HTTP.Conduit" documentation on how to
construct 'C.Request's. Here, we highlight some important details:

* The headers @Content-Length@ and @Host@ are automatically set, and
  should not be added to 'requestHeaders'.

* By default, the functions in this package will /not/ throw
  exceptions for non-2xx status codes. If you would like to use the
  default http-conduit behavior, you should use 'checkStatus', e.g.:

>  req <- parseUrl mapBase
>  resp <- simpleGetHttp $ req { checkStatus = \s@(Status sci _) hs ->
>            if 200 <= sci && sci < 300
>                then Nothing
>                else Just $ toException $ StatusCodeException s hs }

-}

module Hails.HttpClient (
    -- * Request type
    Request
  , method, secure, host, port, path, queryString
  , requestHeaders
  , requestBody, rawBody
  , redirectCount
  , checkStatus, decompress
  , module Network.HTTP.Types
    -- * Response type
  , Response(..)
    -- * Simple HTTP interface
  , parseUrl
  , applyBasicAuth
  , simpleHttp, simpleHttpP
  , simpleGetHttp, simpleGetHttpP
  , simpleHeadHttp, simpleHeadHttpP
  -- * Exceptions
  , HttpException(..)
  ) where

import qualified Data.ByteString.Char8 as S8
import           Data.Monoid
                              
import           Control.Failure
import           Control.Exception

import qualified Network.HTTP.Conduit as C
import           Network.HTTP.Conduit (
                     method, secure, host, port, path, queryString
                   , requestHeaders
                   , requestBody, rawBody
                   , redirectCount
                   , checkStatus, decompress
                   , proxy
                   , applyBasicAuth
                   , HttpException(..)
                   )
import           Hails.HttpServer (Response(..))
import           Network.HTTP.Types

import           LIO
import           LIO.TCB
import           LIO.DCLabel


-- | Reques type, wrapper for the conduit 'C.Request'.
type Request = C.Request

--
-- Basic functions
--

-- | Perform a simple HTTP(S) request.
simpleHttp :: Request  -- ^ Request
           -> DC Response
simpleHttp = simpleHttpP noPrivs

-- | Same as 'simpleHttp', but uses privileges.
simpleHttpP :: PrivDesc DCLabel p
            => Priv p      -- ^ Privilege
            -> Request     -- ^ Request
            -> DC Response
simpleHttpP p req' = do
  let req = req' { proxy = Nothing }
  guardWriteURLP p req
  resp <- ioTCB $ C.withManager $ C.httpLbs req
  return $ Response { respStatus  = C.responseStatus resp
                    , respHeaders = C.responseHeaders resp
                    , respBody    = C.responseBody resp
                    }

-- 
-- Simple HEAD/GET Wrappers
--

-- | Simple HTTP GET request.
simpleGetHttpP :: DCPriv     -- ^ Privilege
               -> String     -- ^ URL
               -> DC Response
simpleGetHttpP p url = do
  req <- parseUrl url
  simpleHttpP p req

-- | Simple HTTP GET request.
simpleGetHttp :: String -> DC Response
simpleGetHttp = simpleGetHttpP mempty

-- | Simple HTTP HEAD request.
simpleHeadHttpP :: DCPriv     -- ^ Privilege
                -> String     -- ^ URL
                -> DC Response
simpleHeadHttpP p url = do
  req <- parseUrl url
  simpleHttpP p $ req { method = methodHead }

-- | Simple HTTP HEAD request.
simpleHeadHttp :: String -> DC Response
simpleHeadHttp = simpleHeadHttpP mempty


--
-- Misc
--

-- | Check that current label can flow to label of request.
guardWriteURLP :: PrivDesc DCLabel p => Priv p -> Request -> DC ()
guardWriteURLP p req = do
  let (lr, lw) = labelOfReq req
  guardAllocP p lr
  taintP p lw

-- | Return the labels corresponding to the absolute URI of a request header.
-- The created labels will have the scheme and authority (including port) in the
-- secrecy componenet, and @|True@ in the integrity component for the
-- read label (and the dual for write label). Specifically, the
-- labels will have the form:
--
--  > (< scheme://authority, |True >,< |True, scheme://authority >)
--
--  For example, the read label of a request to \"http:\/\/gitstar.com/\" is:
-- 
--  > <  "http://gitstar.com:80" , |True>
--
--  while the read label of \"https:\/\/gitstar.com:444/\"
--
--  > <  "https://gitstar.com:444" , |True>
--
-- This should be used for only for single-connection requests, where the
-- absolute URL makes senes.
labelOfReq :: Request -> (DCLabel, DCLabel)
labelOfReq req =
  let scheme = if secure req then (S8.pack "https://") else (S8.pack "http://")
      prin = principalBS $ S8.concat [scheme, host req, S8.pack ":", S8.pack $ show (port req)]
  in (prin %% True, True %% prin)

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting
-- method to GET and 'requestHeaders' to [].
--
parseUrl :: String -> DC Request
parseUrl = C.parseUrl

instance Exception e => Failure e (LIO DCLabel) where
  failure = throwLIO
