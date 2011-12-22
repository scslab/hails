{-# LANGUAGE FlexibleContexts #-}

{- |
Exports basic HTTP client functions inside the 'DC' Monad. Processors are
allowed to communicate over HTTP with the network as long as their label
can flow to \<{target_domain}, False\>, where 'target_domain' is the domain
name or IP address used in the request. Practically, this means library
functions can be called from untrusted code can export data so long as the
untrusted code has not viewed any sensitive data (label higher than public)
or has viewed only sensitive data which has been explicitly labeled to
allow to be exported to the target domain.

For example, assuming some piece of data has been labeled in the following
way:
@
 aliceLocationL = newDC ("alice" .\/. ("maps.googleapis.com" ./\. "https")) (<>)
 myLocation = label aliceLocationL "3101 24th Street, San Francisco, CA"
@
Untrusted code may perform the following operation:
@
 let gMapsBase = "https://maps.googleapis.com/maps/api/geocode/json?sensor=false"
 aliceLocation <- fmap urlEncode myLocation
 resp <- lSimpleHTTP $ getRequest $ gMapsBase ++ "&address=" ++ aliceLocation
@
But an 'LerrHigh' exception will be thrown by the following code:
@
 let gMapsBase = "https://maps.evilalternatives.org/geocode/json?sensor=false"
 aliceLocation <- fmap urlEncode myLocation
 resp <- lSimpleHTTP $ getRequest $ gMapsBase ++ "&address=" ++ aliceLocation
@
-}
module Hails.Network.HTTP (
  module Network.HTTP,
  -- * Functions
  simpleHTTP,
  simpleHTTPWithP,
) where

import Prelude hiding (catch)

import Network.HTTP hiding (simpleHTTP)
import qualified Network.HTTP as Net (simpleHTTP)
import Network.URI
import DCLabel.NanoEDSL
import LIO.DCLabel
import LIO.TCB

-- |Return the label for a given request of the form:
--  @
--    \<{scheme /\\ domain}, False\>
--  @
labelForUri :: HStream ty => Request ty -> DCLabel
labelForUri request = maybe (newDC (_scheme) (<>))
                            (\domain -> newDC (_scheme ./\. domain) (<>))
                            regName
  where regName = fmap uriRegName $ uriAuthority $ rqURI request
        _scheme = takeWhile (/= ':') $ (uriScheme . rqURI) request

-- |Wraps 'Network.HTTP.simpleHTTP', but checks that the current label can flow
--  to \<{schema ./\\. domain}, False\>, where schema is one of 'http' or
--  'https', and domain is the target domain name or IP address in the request.
--  If it is not a valid flow, 'simpleHTTP' will throw 'LerrHigh'.
simpleHTTP :: HStream ty => Request ty -> DC ty
simpleHTTP req = catch (simpleHTTPWithP NoPrivs req) reThrow
  where reThrow LerrPriv = throwIO LerrHigh
        reThrow other = throwIO other

-- |Same as 'simpleHTTP' except checks the flow given /privilege/. If the flow
--  is not valid, it throws 'LerrPriv' instead of 'LerrHigh'.
simpleHTTPWithP :: (Priv DCLabel p, HStream ty) => p -> Request ty -> DC ty
simpleHTTPWithP privilege request = do
  current <- seq request getLabel
  if leqp privilege current (labelForUri request) then
    ioTCB $ do
      resp <- Net.simpleHTTP request
      getResponseBody resp
    else
      throwIO LerrPriv
  