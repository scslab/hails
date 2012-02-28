{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE FlexibleContexts #-}

{- |

Exports basic HTTP client functions inside the 'DC' Monad.
Computations are allowed to communicate over HTTP as long as they can
read and write to a labeled origin. A labeled origin has a label of
the form
@\< {scheme \/\\ domain \/\\ [\"#port\" \\\/ port]}, True \>@, where
@scheme@ is either \'http\' or \'https\', @domain@ is the domain name
or IP address used in the request, and @port@ is the port number of
the connection. In other words, the secrecy component contains the
origin information, while the integrity component is the same as that
of public data.

This means that 'LIO' (specifically, 'DC') computations can export
data if the current label is the same as that of the labeled origin.
Practically, this means that untrusted computation can export data so
long as the they have not observed any data more sensitive than the
label of the target domain.
                                            
For example, suppose some piece of data, @myLoc@, has the label:

> aliceLocL = newDC ("alice" ./\. ("http" ./\. "maps.googleapis.com" ./\. ("#port" .\/. "80"))) (<>)

created as:

> myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"


Then, untrusted code (with initial label set to public) running on
behalf of \"alice\" , may perform the following operation:

> let mapBase = "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
> aliceLoc <- urlEncode <$> (unlabelP alicePriv myLoc)
> resp <- simpleHTTP . getRequest $ mapBase ++ "&address=" ++ aliceLoc

In this case the 'unlabelP' will raise the current label to the label:

> < {"http" /\ "maps.googleapis.com" /\ ["#port" .\/. "80"]}, True >

by exercising \"alice\"\'s privilges.  Directly, the 'simpleHTTPP'
will be permitted. However, if

> let mapBase = "http://maps.evilalternatives.org/geocode/json?sensor=false"

an exception will be thrown since the current label does not flow to
the label of @mapBase@.
-}

module Hails.Network.HTTP ( module Network.HTTP
                          , labelOfURI
                          , simpleHTTP
                          , simpleHTTPP
                          ) where

import Prelude hiding (catch)

import Network.HTTP hiding (simpleHTTP)
import qualified Network.HTTP as Net (simpleHTTP)
import Network.URI
import Network.Stream ( ConnError(..)
                      , Result )
import DCLabel.NanoEDSL
import LIO.DCLabel
import LIO.TCB

-- | Return the label corresponding to a URI. The created label with
-- have the form:
--
--  > < {scheme /\ domain /\ ["#port" \/ port]}, True >
--
--  For example, the label of \"http:\/\/gitstar.com/\"
--  will be:
-- 
--  > <{["#port" \/ "80"] /\ ["gitstar.com"] /\ ["https"]} , True>
--
--  while the label of \"https:\/\/gitstar.com:444/\"
--
--  > <{["#port" \/ "444"] /\ ["gitstar.com"] /\ ["https"]} , True>
--
labelOfURI :: URI -> Maybe DCLabel
labelOfURI uri = do
  auth <- uriAuthority uri
  schm <- notNull $ takeWhile (/= ':') $ uriScheme uri
  domain <- notNull $ uriRegName auth
  let port'  = dropWhile (== ':') $ uriPort auth
  prt  <- if not $ null port' then Just port' else defaultPort schm
  return $ newDC (schm ./\. domain ./\. ("#port" .\/. prt)) (<>)
    where notNull s | null s    = Nothing
                    | otherwise = Just s
          defaultPort s | s == "http"  = Just "80"
                        | s == "https" = Just "443"
                        | otherwise    = Nothing

-- | Wraps 'Network.HTTP.simpleHTTP', but checks that the current label can flow
-- to @\<{schema ./\\. domain ./\\. [\"#port\" .\\/. port ] }, True\>@,
-- where @schema@ is one of \'http\' or \'https\', @domain@ is the target
-- domain name or IP address in the request, and @port@ is the port
-- number (default is @80@ for \'http\' and @443@ for \'https\').
-- Because the function returns a response, the current label is also
-- raised to reflect a read of data with such a label. Internally
-- @simpleHTTP@ uses 'wguard'.
simpleHTTP :: HStream ty => Request ty -> DC (Result (Response ty))
simpleHTTP = simpleHTTPP noPrivs

-- | Same as 'simpleHTTP' but uses privileges when comparing labels.
simpleHTTPP :: HStream ty
            => DCPrivTCB -> Request ty -> DC (Result (Response ty))
simpleHTTPP p' req = withCombinedPrivs p' $ \p ->
  case labelOfURI (rqURI req) of
    Nothing -> return . Left $ ErrorParse "Cannot create URI label"
    Just lURI -> do
      wguardP p lURI
      rtioTCB $ Net.simpleHTTP req
