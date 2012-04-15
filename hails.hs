{-# LANGUAGE ScopedTypeVariables #-}
import Hails.TCB.Load
import Hails.HttpServer
import Hails.HttpServer.Auth

import Data.IterIO.Server.TCPServer
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe)

import System.Environment
import System.Console.GetOpt
import System.IO (stderr, hPutStrLn)
import System.Exit

import Control.Monad (when)


version :: String
version = "0.0"

about :: String -> String -> String
about prog ver = "About: " ++ prog ++ " " ++ ver ++
  " \n Simple tool for launching HAILS apps. \
  \ By default, " ++ prog ++ " uses the environment \
  \ variables APP_NAME and PORT. Use the command-line \
  \ arguments to override."

--
--
--

main :: IO ()
main = do
  args <- getArgs
  env  <- getEnvironment
  opts <- hailsOpts args env
  when (optAbout opts) $ printAbout
  let appName = optName opts
      port    = optPort opts
      authF   = if optDev opts
                  then basicNoAuth
                  else basicNoAuth
  func <- loadApp appName
  runTCPServer $ secureHttpServer authF (fromInteger port) func

--
-- Helper
--

printAbout :: IO ()
printAbout = do
  prog <- getProgName
  putStrLn $ about prog version
  exitSuccess


--
-- Parsing options
--

data Options = Options 
   { optName   :: String  -- ^ App name
   , optPort   :: Integer -- ^ App port number
   , optAbout  :: Bool    -- ^ About this program
   , optDev    :: Bool    -- ^ Development
   }

defaultOpts :: Options
defaultOpts = Options { optName   = "App"
                      , optPort   = 8080
                      , optAbout  = False
                      , optDev    = False }

options :: [ OptDescr (Options -> Options) ]
options = 
  [ Option ['s'] ["start"]
      (ReqArg (\n o -> o { optName = n }) "APP_NAME")
      "Start application APP_NAME"
  , Option ['p'] ["port"]
      (ReqArg (\p o -> o { optPort = read p }) "PORT")
      "Application PORT"
  , Option ['d']    ["dev", "development"]
        (NoArg (\opts -> opts { optDev = True }))
        "about this program"
  , Option ['h','?']    ["help", "about"]
        (NoArg (\opts -> opts { optAbout = True }))
        "about this program"
  ]

hailsOpts :: [String] -> [(String, String)] -> IO Options
hailsOpts args env = do
  let opts = envOpts defaultOpts env
  case getOpt Permute options args of
    (o,[], []) -> return $ foldl (flip id) opts o
    (_,_,errs) -> do prog <- getProgName
                     hPutStrLn stderr $ concat errs ++
                                        usageInfo (header prog) options
                     exitFailure
    where header prog = "Usage: " ++ prog ++ "[OPTION...]"


envOpts :: Options -> [(String, String)] -> Options
envOpts opts env = 
  opts { optName = maybe (optName opts) id (fromEnv "APP_NAME")
       , optPort = maybe (optPort opts) id (readFromEnv "PORT")
       }
    where fromEnv n = lookup n env
          readFromEnv n = lookup n env >>= mRead
          mRead :: Read a => String -> Maybe a
          mRead s = fst <$> (listToMaybe $ reads s)




