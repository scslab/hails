module Main (main) where
import LIO
import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.Privs.TCB

import Hails.HttpClient

dcPutStrLn :: String -> DC ()
dcPutStrLn s = ioTCB $ putStrLn s

alicePriv :: DCPriv
alicePriv = mintTCB (dcPrivDesc "alice")

evalDC' :: DC () -> IO ()
evalDC' io = do
  (_, s) <- runDC io
  putStrLn $ show $ lioLabel s

main :: IO ()
main = evalDC' $ do
  case exNr of
    1 {- OK -}     -> exMap False "maps.googleapis.com" mapBase
    2 {- OK SSL -} -> exMap True  "maps.googleapis.com" mapBaseS
    3 {- FAIL -}   -> exMap False "maps.yahoo.com"      mapBase
    4 {- FAIL -}   -> exMap False "maps.googleapis.com" "http://maps.google.com"
    5 {- FAIL -}   -> exMap True  "maps.googleapis.com" mapBase
    _ -> return ()
  where exNr = 1 :: Int
        mapBase = "http://maps.googleapis.com/maps/api/geocode/json?sensor=false"
        mapBaseS = "https://maps.googleapis.com/maps/api/geocode/json?sensor=false"

exMap :: Bool -> String -> String -> DC ()
exMap sec domain mapBase = do
  let aliceLocL = dcLabel ("alice" /\  (scheme ++ domain ++ p)) dcTrue
  myLoc <- labelP alicePriv  aliceLocL "3101 24th Street, San Francisco, CA"
  aliceLoc <- unlabelP alicePriv myLoc
  resp <- simpleGetHttp $ mapBase ++ "&address=" ++ aliceLoc
  dcPutStrLn (show resp)
    where scheme = (if sec then "https" else "http") ++ "://"
          p = if sec then ":443" else  ":80"
