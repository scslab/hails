import LIO
import LIO.TCB (ioTCB)
import LIO.DCLabel

import DCLabel.TCB ( createPrivTCB )
import DCLabel.NanoEDSL
import DCLabel.PrettyShow

import Hails.Network.HTTP

import Data.Functor ((<$>))

dcPutStrLn :: String -> DC ()
dcPutStrLn s = ioTCB $ putStrLn s

alicePriv :: DCPrivTCB
alicePriv = createPrivTCB (newPriv "alice")

evalDC' :: DC () -> IO ()
evalDC' io = do
  (_, l) <- evalDC io
  putStrLn $ prettyShow l

main :: IO ()
main = evalDC' $ do
  case exNr of
    1 -> exMap "maps.googleapis.com"
               "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
    2 -> exMap "maps.yahoo.com"    
               "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
    3 -> exMap "maps.googleapis.com"
               "http://maps.google.com"
    _ -> return ()
  where exNr = 1

exMap :: String -> String -> DC ()
exMap domain mapBase = do
  taint lpub
  let aliceLocL = newDC ("alice" ./\.
                          ("http" ./\. domain
                                  ./\. ("#port" .\/. "80"))
                        ) (<>)
  myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"
  aliceLoc <- urlEncode <$> (unlabelP alicePriv myLoc)
  resp <- simpleHTTP . getRequest $ mapBase ++ "&address=" ++ aliceLoc
  case resp of
    Left err -> dcPutStrLn $ "Errorr: " ++ show err
    Right  r -> dcPutStrLn $ rspBody r

