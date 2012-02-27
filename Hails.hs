import Hails.TCB.Load
import Hails.HttpServer
import Data.IterIO.Server.TCPServer
import System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read (lookup "PORT" env)
  let appName = maybe "App" id (lookup "APP_NAME" env)
  func <- loadApp appName
  runTCPServer $ secureHttpServer (fromInteger port) func
