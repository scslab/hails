import GHC
import GHC.Paths
import Unsafe.Coerce

import Hails.HttpServer
import Data.IterIO.Http
import Data.IterIO.Server.TCPServer
import LIO.DCLabel
import System.Environment

main :: IO ()
main = do
	env <- getEnvironment
	let port = maybe 8080 read (lookup "PORT" env)
	let appName = maybe "App" id (lookup "APP_NAME" env)
	func <- loadApp appName
	runTCPServer $ secureHttpServer (fromInteger port) func

loadApp :: String -> IO (DCPrivTCB -> HttpRequestHandler DC ())
loadApp appName = runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	_ <- setSessionDynFlags dflags
	target <- guessTarget appName Nothing
	addTarget target
	r <- load LoadAllTargets
	case r of
	  Failed -> error "Compilation failed"
	  Succeeded -> do
	    setContext [] [simpleImportDecl (mkModuleName appName)]
	    value <- compileExpr (appName ++ ".server")
	    do let value' = (unsafeCoerce value) :: DCPrivTCB -> HttpRequestHandler DC ()
	       return value'