{-# LANGUAGE ScopedTypeVariables #-}
import Hails.TCB.Load
import Hails.HttpServer

import Data.IterIO.Server.TCPServer

import System.Environment
import System.Console.GetOpt
import System.IO (stderr, hPutStrLn)
import System.Exit

import Control.Exception
import Control.Monad (when)


version :: String
version = "0.0"

about :: String -> String -> String
about prog ver = "About: " ++ prog ++ " " ++ ver ++
  " \n Simple tool for launching HAILS apps."

--
--
--

main :: IO ()
main = do
  args <- getArgs
  opts <- parseHailsOpts args
  when (optAbout opts) $ printAbout
  let appName = optName opts
      port = optPort opts
  func <- loadApp appName
  runTCPServer $ secureHttpServer (fromInteger port) func

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
   , optUseEnv :: Bool    -- ^ Use environment for config
   , optAbout  :: Bool    -- ^ About this program
   }

defaultOpts :: Options
defaultOpts = Options { optName   = "App"
                      , optPort   = 8080
                      , optUseEnv = False 
                      , optAbout  = False }

options :: [ OptDescr (Options -> Options) ]
options = 
  [ Option ['E'] ["env","environment"]
      (NoArg (\o -> o { optUseEnv = True }))
      "Use environment for configuration"
  , Option ['s'] ["start"]
      (ReqArg (\n o -> o { optName = n }) "NAME")
      "Start application NAME"
  , Option ['p'] ["port"]
      (ReqArg (\p o -> o { optPort = read p }) "PORT")
      "Application PORT"
  , Option ['h','?']    ["help", "about"]
        (NoArg (\opts -> opts { optAbout = True }))
        "about this program"
  ]

hailsOpts :: [String] -> IO Options
hailsOpts args = do
  case getOpt Permute options args of
    (o,[], []) -> return $ foldl (flip id) defaultOpts o
    (_,_,errs) -> do prog <- getProgName
                     hPutStrLn stderr $ concat errs ++
                                        usageInfo (header prog) options
                     exitFailure
    where header prog = "Usage: " ++ prog ++ "[OPTION...]"


parseHailsOpts :: [String] -> IO Options
parseHailsOpts args = do
  opts <- hailsOpts args
  if optUseEnv opts && (not $ optAbout opts)
    then do n <- getEnv "HAILS_APP_NAME"
            p <- getEnv' "HAILS_PORT"
            return $ opts { optName = n, optPort = p } 
    else return opts
  where getEnv' n = handle (\(e::SomeException) ->
             throwIO . userError $ "Reading \'" ++ n ++ "\' failed: " ++ show e
         ) $ getEnv n >>= readIO




