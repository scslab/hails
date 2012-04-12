{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 704)
{-# LANGUAGE Trustworthy #-}
#endif
module Hails.IterIO.HailsRoute ( routeFileSys
                               , systemMimeMap) where

import Hails.IterIO.Conversions
import qualified Data.ByteString.Char8 as S8
import Data.IterIO
import Data.IterIO.HttpRoute hiding (routeFileSys)
import LIO.TCB
import LIO (liftLIO)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import System.IO.Unsafe
import Control.Exception (SomeException(..))

routeFileSys :: LabelState l p st =>
                (String -> S8.ByteString)
             -- ^ Map of file suffixes to mime types (see 'mimeTypesI')
             -> (FilePath -> HttpRoute (LIO l p st) s)
             -- ^ Handler to invoke when the URL maps to a directory
             -- in the file system.  Reasonable options include:
             --
             -- * @('const' 'mempty')@ to do nothing, which results in a
             --   403 forbidden,
             --
             -- * @('dirRedir' \"index.html\")@ to redirect directory
             --   accesses to an index file, and
             --
             -- * a recursive invocation such as @(routeFileSys
             -- typemap . (++ \"/index.html\"))@ to re-route the
             -- request directly to an index file.
             -> FilePath
             -- ^ Pathname of directory to serve from file system
             -> HttpRoute (LIO l p st) s
routeFileSys = routeGenFileSys defaultFileSystemCalls

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

defaultFileSystemCalls :: LabelState l p st => FileSystemCalls Fd (LIO l p st)
defaultFileSystemCalls = FileSystemCalls { fs_stat = liftLIO . ioTCB . getFileStatus
                                         , fs_open = liftLIO . ioTCB . pathToFd
                                         , fs_close = liftLIO . ioTCB . closeFd
                                         , fs_fstat = liftLIO . ioTCB . getFdStatus
                                         , fs_enum = liftLIO . ioTCB . fdToOnum
                                         }
    where pathToFd path = openFd path ReadOnly Nothing defaultFileFlags
          fdToOnum fd = do h <- fdToHandle fd
                           return $ onumIOtoOnumLIO $ enumHandle h
