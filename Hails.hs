import Hails.TCB.Load
import Hails.HttpServer
import Data.IterIO.Server.TCPServer
import System.Environment
import System.Console.GetOpt
import Network.Socket (PortNumber)

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read (lookup "PORT" env)
  let appName = maybe "App" id (lookup "APP_NAME" env)
  func <- loadApp appName
  runTCPServer $ secureHttpServer (fromInteger port) func


data Options = Options 
   { optName   :: String  -- ^ App name
   , optPort   :: Int     -- ^ App port number
   , optUseEnv :: Bool    -- ^ Use environment for config
   }

defaultOptions :: Options
defaultOptions = Options { optName   = "App"
                         , optPort   = 8080
                         , optUseEnv = False }

options :: [ OptDescr (Options -> Options) ]
options = 
  [ Option ['E'] ["env","environment"]
      (NoArg (\o -> o { optUseEnv = True }))
      "Use environemtn for configuration"
  , Option ['s'] ["start"]
      (ReqArg (\n o -> o { optName = n }) "NAME")
      "Start application NAME"
  , Option ['p'] ["port"]
      (ReqArg (\p o -> o { optPort = read p }) "PORT")
      "Application PORT"
  ]
