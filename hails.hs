{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import           Control.Exception
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.Text as T
import           Data.List (isPrefixOf, isSuffixOf)
import qualified Data.List as List
import           Data.Maybe
import           Data.Version
import           Control.Monad

import           LIO
import           LIO.DCLabel
import           Hails.HttpServer
import           Hails.HttpServer.Auth
import           Hails.Version

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger

import           System.Posix.Env (setEnv)
import           System.Environment
import           System.Console.GetOpt hiding (Option)
import qualified System.Console.GetOpt as GetOpt
import           System.IO (stderr, hPutStrLn)
import           System.FilePath
import           System.Directory
import           System.Exit


import           Language.Haskell.Interpreter
import           Language.Haskell.Interpreter.Extension


about :: String -> String
about prog = prog ++ " " ++ showVersion version ++                   "\n\n" ++
  concat [ "Simple tool for launching Hails apps.  This tool can be used in\n"
         , "both development and production mode.  It allows you configure the\n"
         , "environment your app runs in (e.g., the port number the Hails HTTP\n"
         , "server should listen on, the MongoDB server it should connect to,\n"
         , "etc.). In development mode (default), "
         , prog
         , " uses some default\n"
         , "settings (e.g., port 8080).  In production, mode all configuration\n"
         , "settings must be specified.  To simplify deployment, this tool\n"
         , "checks the program environment for configuration settings (e.g.,\n"
         , "variable PORT is used for the port number), but you can override\n"
         , "these with arguments. See \'"
         , prog
         , " --help\' for a list of     \n"
         , "configuration settings and corresponding environment variables.\n\n"
         , prog
         , " dynamically loads your app requst handler. Hence, the\n"
         , "app name is the module name where your \'server\' function is\n"
         , "defined."]

--
--
--


main :: IO ()
main = do
  args <- getArgs
  env  <- getEnvironment
  opts <- do opts <- hailsOpts args env
             when (optAbout opts) $ printAbout
             opts' <- case optInFile opts of
               Nothing -> return opts
               Just file -> do envFromFile file
                               env' <- getEnvironment
                               print env'
                               hailsOpts args env'
             cleanOpts opts'
  maybe (return ()) (optsToFile opts) $ optOutFile opts
  putStrLn $ "Working environment:\n\n" ++ optsToEnvStr opts
  forM_ (optsToEnv opts) $ \(k,v) -> setEnv k v True
  let port = fromJust $ optPort opts
      hmac_key = L8.pack . fromJust $ optHmacKey opts
      persona = personaAuth hmac_key $ T.pack . fromJust . optPersonaAud $ opts
      openid  = openIdAuth  $ T.pack . fromJust . optOpenID $ opts
      external  = externalAuth  hmac_key
                                (fromJust $ optExternal $ opts)
      logMiddleware  = if optDev opts then logStdoutDev  else logStdout
      authMiddleware = case () of
         -- dev/production mode with persona:
         _ | isJust (optPersonaAud opts) -> persona
         -- dev/productoin mode with openid:
         _ | isJust (optOpenID opts)     -> openid
         _ | isJust (optExternal opts)     -> external
         -- dev mode:
         _                               -> devBasicAuth
  dcApp <- loadApp (optSafe opts) (optPkgConf opts) (fromJust $ optName opts)
  app <- evalDC $ setClearance dcPublic >> dcApp
  runSettings (defaultSettings { settingsPort = port }) $
    logMiddleware $ execHailsApplication authMiddleware app


-- | Given an application module name, load the main controller named
-- @server@.
loadApp :: Bool             -- -XSafe ?
        -> Maybe FilePath   -- -package-db
        -> String           -- Application name
        -> IO (DC Application)
loadApp safe mpkgDb appName = do
  case mpkgDb of
    Just pkgDb -> setEnv "GHC_PACKAGE_PATH" pkgDb True
    Nothing -> return ()
  eapp <- runInterpreter $ do
    when safe $
      set [languageExtensions := [asExtension "Safe"]]
    loadModules [appName]
    setImports  ["LIO", "LIO.DCLabel", "Hails.HttpServer", appName]
    entryFunType <- typeOf "server"
    if entryFunType == "DC Application" then
      interpret "server" (undefined :: DC Application)
      else
      interpret "P.return server" (undefined :: DC Application)
  case eapp of
    Left err -> throwIO err
    Right app -> return app

--
-- Parsing options
--

-- | Type used to encode hails options
data Options = Options
   { optName        :: Maybe String  -- ^ App name
   , optPort        :: Maybe Int     -- ^ App port number
   , optAbout       :: Bool          -- ^ About this program
   , optSafe        :: Bool          -- ^ Use @-XSafe@
   , optForce       :: Bool          -- ^ Force unsafe in production
   , optDev         :: Bool          -- ^ Development/Production
   , optExternal    :: Maybe String  -- ^ External Auth URL
   , optOpenID      :: Maybe String  -- ^ OpenID provider
   , optHmacKey     :: Maybe String  -- ^ HMAC cookie key
   , optPersonaAud  :: Maybe String  -- ^ Persona audience
   , optDBConf      :: Maybe String  -- ^ Filepath of databases conf file
   , optPkgConf     :: Maybe String  -- ^ Filepath of package-conf
   , optMongoServer :: Maybe String  -- ^ MongoDB server URL
   , optCabalDev    :: Maybe String  -- ^ Cabal-dev directory
   , optOutFile     :: Maybe String  -- ^ Write configurate to file
   , optInFile      :: Maybe String  -- ^ Read configurate from file
   } deriving Show

-- | Default options
defaultOpts :: Options
defaultOpts = Options { optName        = Nothing
                      , optPort        = Nothing
                      , optAbout       = False
                      , optSafe        = True
                      , optForce       = False
                      , optDev         = True
                      , optExternal    = Nothing
                      , optOpenID      = Nothing
                      , optHmacKey     = Nothing
                      , optPersonaAud  = Nothing
                      , optDBConf      = Nothing
                      , optPkgConf     = Nothing
                      , optCabalDev    = Nothing
                      , optMongoServer = Nothing 
                      , optOutFile     = Nothing
                      , optInFile      = Nothing}

-- | Default development options. These options can be used 
-- when in development mode, to avoid annoying the user.
defaultDevOpts :: Options
defaultDevOpts = Options { optName        = Just "App"
                         , optPort        = Just 8080
                         , optAbout       = False
                         , optSafe        = True
                         , optForce       = False
                         , optDev         = True
                         , optExternal    = Nothing
                         , optOpenID      = Nothing
                         , optHmacKey     = Just "hails-d34adb33f-key"
                         , optPersonaAud  = Nothing
                         , optDBConf      = Just "database.conf"
                         , optPkgConf     = Nothing
                         , optCabalDev    = Nothing
                         , optMongoServer = Just "localhost"
                         , optOutFile     = Nothing
                         , optInFile      = Nothing}


-- | Parser for options
options :: [ OptDescr (Options -> Options) ]
options = 
  [ GetOpt.Option ['a'] ["app"]
      (ReqArg (\n o -> o { optName = Just n }) "APP_NAME")
      "Start application APP_NAME."
  , GetOpt.Option ['p'] ["port"]
      (ReqArg (\p o -> o { optPort = Just $ read p }) "PORT")
      "Run application on port PORT."
  , GetOpt.Option []    ["dev", "development"]
        (NoArg (\opts -> opts { optDev = True }))
        "Development mode, default (no authentication)."
  , GetOpt.Option []    ["prod", "production"]
        (NoArg (\opts -> opts { optDev = False }))
        "Production mode (Persona/OpenID authentication). Must set OPENID_PROVIDER or PERSONA_AUDIENCE."
  , GetOpt.Option [] ["openid-provider"]
      (ReqArg (\u o -> o { optOpenID = Just u }) "OPENID_PROVIDER")
      "Set OPENID_PROVIDER as the OpenID provider."
  , GetOpt.Option [] ["persona-audience"]
      (ReqArg (\u o -> o { optPersonaAud = Just u }) "PERSONA_AUDIENCE")
      "Set PERSONA_AUDIENCE as the persona audience (webserver sheme://host:prot)."
  , GetOpt.Option [] ["hmac-key"]
      (ReqArg (\u o -> o { optHmacKey = Just u }) "HMAC_KEY")
      "Set HMAC_KEY as the MAC key for cookies." 
  , GetOpt.Option []    ["unsafe"]
        (NoArg (\opts -> opts { optSafe = False }))
        "Turn the -XSafe flag off."
  , GetOpt.Option []    ["force"]
        (NoArg (\opts -> opts { optForce = True }))
        "Use with --unsafe to force the -XSafe flag off in production mode."
  , GetOpt.Option [] ["package-conf"]
      (ReqArg (\n o -> o { optPkgConf = Just n }) "PACKAGE_CONF")
        "Use PACKAGE_CONF for as the app specific package-conf file."
  , GetOpt.Option ['s'] ["cabal-dev"]
      (ReqArg (\n o -> o { optCabalDev = Just n }) "CABAL_DEV_SANDBOX")
        "The location ofthe cabal-dev sandbox (e.g., ./cabal-dev)."
  , GetOpt.Option [] ["db-conf", "database-conf"]
      (ReqArg (\n o -> o { optDBConf = Just n }) "DATABASE_CONFIG_FILE")
        "Use DATABASE_CONFIG_FILE  as the specific database.conf file."
  , GetOpt.Option [] ["db", "mongodb-server"]
      (ReqArg (\n o -> o { optMongoServer = Just n }) "HAILS_MONGODB_SERVER")
        "Use HAILS_MONGODB_SERVER as the URL to the MongoDB server."
  , GetOpt.Option [] ["out"]
      (ReqArg (\n o -> o { optOutFile = Just n }) "OUT_FILE")
        "Write options to environment file OUT_FILE."
  , GetOpt.Option [] ["in", "env", "environment"]
      (ReqArg (\n o -> o { optInFile = Just n }) "IN_FILE")
        "Load environment variables from file IN_FILE."
  , GetOpt.Option ['?']    ["about"]
        (NoArg (\opts -> opts { optAbout = True }))
        "About this program."
  ]

-- | Do parse options
hailsOpts :: [String] -> [(String, String)] -> IO Options
hailsOpts args env =
  let opts = envOpts defaultOpts env
  in case getOpt Permute options args of
       (o,[], []) -> return $ foldl (flip id) opts o
       (_,_,errs) -> do prog <- getProgName
                        hPutStrLn stderr $ concat errs ++
                                           usageInfo (header prog) options
                        exitFailure
    where header prog = "Usage: " ++ prog ++ " [OPTION...]"


-- | Extracting options from the environment (prioritzed) over
-- arguments
envOpts :: Options -> [(String, String)] -> Options
envOpts opts env = 
  opts { optName        = mFromEnvOrOpt "APP_NAME" optName
       , optPort        = case readFromEnv "PORT" of
                            p@(Just _) -> p
                            _ -> optPort opts
       , optOpenID      = mFromEnvOrOpt "OPENID_PROVIDER" optOpenID 
       , optPersonaAud  = mFromEnvOrOpt "PERSONA_AUDIENCE" optPersonaAud
       , optExternal    = mFromEnvOrOpt "AUTH_URL" optPersonaAud
       , optHmacKey     = mFromEnvOrOpt "HMAC_KEY" optHmacKey
       , optDBConf      = mFromEnvOrOpt "DATABASE_CONFIG_FILE" optDBConf
       , optPkgConf     = mFromEnvOrOpt "PACKAGE_CONF" optPkgConf
       , optCabalDev    = mFromEnvOrOpt "CABAL_DEV_SANDBOX" optCabalDev
       , optMongoServer = mFromEnvOrOpt "HAILS_MONGODB_SERVER" optMongoServer
       }
    where fromEnv n = lookup n env
          readFromEnv n = lookup n env >>= mRead
          mRead :: Read a => String -> Maybe a
          mRead s = fst `liftM` (listToMaybe $ reads s)
          mFromEnvOrOpt evar f = case fromEnv evar of
                                   x@(Just _) -> x
                                   _ -> f opts

cleanOpts :: Options -> IO Options
cleanOpts opts = do
  when (optAbout opts) $ printAbout
  if optDev opts 
    then cleanDevOpts opts
    else cleanProdOpts opts

-- | Clean options and use default development options when
-- non-existant.
cleanDevOpts :: Options -> IO Options
cleanDevOpts opts0 = do
  let opts1 = opts0 { optName        = mergeMaybe optName
                    , optPort        = mergeMaybe optPort
                    , optOpenID      = mergeMaybe optOpenID
                    , optPersonaAud  = mergeMaybe optPersonaAud
                    , optExternal    = mergeMaybe optExternal
                    , optHmacKey     = mergeMaybe optHmacKey
                    , optDBConf      = mergeMaybe optDBConf
                    , optMongoServer = mergeMaybe optMongoServer }
  case (optPkgConf opts1, optCabalDev opts1) of
    (Just _, Just _) -> do
      hPutStrLn stderr "Flag package-conf supplied, ignoring cabal-dev sandbox"
      return $ opts1 { optCabalDev = Nothing }
    (_, Just cd) -> do
      pkgConf <- findPackageConfInCabalDev cd
      return $ opts1 { optCabalDev = Nothing, optPkgConf = Just pkgConf }
    _ -> return opts1
  where mergeMaybe f = f $ if isJust (f opts0)
                             then opts0
                             else defaultDevOpts

-- | Clean options and strictly check that all the necessary ones
-- exist.
cleanProdOpts :: Options -> IO Options
cleanProdOpts opts0 = do
  checkIsJust [(optName        ,"APP_NAME"            )]
  checkIsJust [(optPort        ,"PORT"                )]
  checkIsJust [(optOpenID      ,"OPENID_PROVIDER"     ) {- or -}
              ,(optPersonaAud  ,"PERSONA_AUDIENCE"    ) {- or -}
              ,(optExternal    ,"AUTH_URL")]
  when (isJust $ optPersonaAud opts0) $ checkIsJust [(optHmacKey ,"HMAC_KEY")]
  checkIsJust [(optDBConf      ,"DATABASE_CONFIG_FILE")]
  checkIsJust [(optMongoServer ,"HAILS_MONGODB_SERVER")]
  when ((isJust $ optPersonaAud opts0) && (isJust $ optOpenID opts0)) $ do
    hPutStrLn stderr "Both OpenID and Persona are set."
    exitFailure
  unless (optSafe opts0 || optForce opts0) $ do
    hPutStrLn stderr "Production code must be Safe, use --force to override"
    exitFailure
  case (optPkgConf opts0, optCabalDev opts0) of
    (Just _, Just _) -> do
      hPutStrLn stderr "Both package-conf supplied and cabal-dev sandbox defined."
      exitFailure
    (_, Just cd) -> do
      pkgConf <- findPackageConfInCabalDev cd
      return $ opts0 { optCabalDev = Nothing, optPkgConf = Just pkgConf }
    _ -> return opts0
    where checkIsJust fs = 
            unless (any (\f -> isJust $ (fst f) opts0) fs) $ do
              let msg = List.intercalate " or " $ map snd fs
              hPutStrLn stderr $ "Production mode is strict, missing " ++ msg
              exitFailure


-- | Find the package-conf file in a cabal-dev directory (e.g.,
-- packages-7.4.2.conf)
findPackageConfInCabalDev :: FilePath -> IO FilePath
findPackageConfInCabalDev cdev = do
  fs <- getDirectoryContents cdev
  case filter f fs of
    []     -> do
      hPutStrLn stderr $ "Could not file package config file in " ++ show cdev
      exitFailure
    xs@(x:_)  -> do 
      let path = cdev </> x
      when (length xs > 1) $ hPutStrLn stderr $ "Using " ++ show path ++
                                                " for the package config file"
      return path
  where f d = "packages-" `isPrefixOf` d && ".conf" `isSuffixOf` d
  
-- | Print about message
printAbout :: IO ()
printAbout = do
  prog <- getProgName
  putStrLn $ about prog
  exitSuccess

-- | Write options to environment file
optsToFile :: Options -> FilePath -> IO ()
optsToFile opts file = writeFile file (optsToEnvStr opts) >> exitSuccess

-- | Options to envionment string
optsToEnv :: Options -> [(String,String)]
optsToEnv opts = map (\(k, mv) -> (k, fromJust mv)) $ 
                 filter (isJust . snd) $
  [toLine optName        "APP_NAME"
  ,("PORT", show `liftM` optPort opts)
  ,toLine optOpenID      "OPENID_PROVIDER"
  ,toLine optPersonaAud  "PERSONA_AUDIENCE"
  ,toLine optHmacKey     "HMAC_KEY"
  ,toLine optDBConf      "DATABASE_CONFIG_FILE"
  ,toLine optMongoServer "HAILS_MONGODB_SERVER"
  ,toLine optPkgConf     "PACKAGE_CONF"
  ,toLine optCabalDev    "CABAL_DEV_SANDBOX" ]
    where toLine f var = (var, f opts)

-- | Create environment list
optsToEnvStr :: Options -> String
optsToEnvStr opts = unlines $ map (\(k,v) -> k++"="++v) $ optsToEnv opts 

-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
envFromFile :: FilePath -> IO ()
envFromFile file = do
  ls <- S8.lines `liftM` S8.readFile file
  forM_ ls $ \line ->
    let (key',val') = S8.span (/='=') line
        val = safeTail val'
    in case S8.words key' of
         [key] -> setEnv (S8.unpack key) (S8.unpack val) True
         _ -> do hPutStrLn stderr $ "Invalid environment line: " ++
                                    show (S8.unpack line)
                 exitFailure
      where safeTail s = if S8.null s then s else S8.tail s 
