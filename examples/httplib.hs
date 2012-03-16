import LIO
import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.LIORef

import DCLabel.TCB ( createPrivTCB )
import DCLabel.NanoEDSL
import DCLabel.PrettyShow

import Control.Monad.Trans (lift)
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe)

import Data.IterIO
import Hails.IterIO.HttpClient
import Hails.IterIO.Conversions ( iterIOtoIterLIO )

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP (urlEncode)

import OpenSSL

dcPutStrLn :: String -> DC ()
dcPutStrLn s = ioTCB $ putStrLn s

alicePriv :: DCPrivTCB
alicePriv = createPrivTCB (newPriv "alice")

evalDC' :: DC () -> IO ()
evalDC' io = do
  (_, l) <- evalDC io
  putStrLn $ prettyShow l

main :: IO ()
main = withOpenSSL$ evalDC' $ do
  case exNr of
    1 -> exMap "maps.googleapis.com"
               "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
    2 -> exMap "maps.yahoo.com"    
               "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
    3 -> exMap "maps.googleapis.com"
               "http://maps.google.com"
    4 -> exMultiMap "maps.googleapis.com"
            "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
    _ -> return ()
  where exNr = 4

exMap :: String -> String -> DC ()
exMap domain mapBase = do
  taint lpub
  let aliceLocL = newDC ("alice" ./\.  ("http://" ++ domain ++ ":80")) (<>)
  myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"
  aliceLoc <- urlEncode <$> (unlabelP alicePriv myLoc)
  resp <- simpleGetHttp $ mapBase ++ "&address=" ++ aliceLoc
  enumBody <- respBodyDC resp
  enumBody |$ stdoutLIO


exMultiMap domain mapBase = do
  taint lpub
  let myLoc0 = urlEncode "3101 24th Street, San Francisco, CA"
      myLoc1 = urlEncode "3102 24th Street, San Francisco, CA"
      req0   = getRequest (mapBase ++ "&address=" ++ myLoc0)
      req1   = getRequest (mapBase ++ "&address=" ++ myLoc1)
      l       = newDC ("alice" ./\.  ("http://" ++ domain ++ ":80")) (<>)
  reqs <- newLIORefP alicePriv l [req0, req1]
  multiHttp  (getRequest mapBase, noBody) $ \resp -> lift $ do
    enumPure (L8.pack "here\n") |$ stdoutLIO
    enumBody <- respBodyDC resp
    enumBody |$ stdoutLIO
    getNextReqP alicePriv reqs
    where noBody = L.empty
          getNextReqP p r = do
            rs <- readLIORefP p r
            writeLIORefP p r (safeTail rs)
            return $ do r <- listToMaybe rs
                        return (r, noBody)
-- | Safe 'tail'
safeTail [] = []
safeTail (_:x) = x

-- | Write to stdout
stdoutLIO :: Iter L.ByteString DC ()
stdoutLIO = iterIOtoIterLIO stdoutI
