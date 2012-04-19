{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 704)
{-# LANGUAGE Trustworthy #-}
#endif
module Hails.IterIO.Mime ( systemMimeMap ) where

import qualified Data.ByteString.Char8 as S8
import Data.IterIO
import Data.IterIO.HttpRoute hiding (routeFileSys)
import System.IO.Unsafe
import Control.Exception (SomeException(..))

-- | Given a file extension (e.g., \"hs\") return its MIME type (e.g.,
-- \"text\/x-haskell\"). If there is no recognized MIME type (or none
-- of the default paths exist), this function returns
-- \"application\/octet-stream\"
systemMimeMap :: String -> S8.ByteString
systemMimeMap = unsafePerformIO $ 
  foldr1 cat (map enumMimeFile defaultPaths) |$
    mimeTypesI "application/octet-stream"
    where defaultPaths = ["mime.types"
                         , "/etc/mime.types"
                         , "/var/www/conf/mime.types"]
          enumMimeFile f = inumCatch (enumFile f) $ \(SomeException _) res ->
                                                      resumeI res
