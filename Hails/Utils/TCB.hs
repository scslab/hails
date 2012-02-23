{-# LANGUAGE Unsafe #-}

module Hails.Utils.TCB where

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

import Data.IterIO.Http
import LIO.DCLabel
import DCLabel.Core

loadDatabase :: String -> String -> FilePath -> IO a
loadDatabase privName policyMod policyFile = runGhc (Just libdir) $ do
  let policyPriv = createPrivTCB $ newPriv privName
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags $ dflags { safeHaskell = Sf_Safe }
  let target = Target (TargetFile policyFile Nothing) False Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
    Failed -> error "Compilation failed"
    Succeeded -> do
      setContext [IIDecl $ simpleImportDecl (mkModuleName policyMod)]
      value <- fmap unsafeCoerce $ compileExpr "withDB"
      return $ value policyPriv


loadApp :: String -> IO (DCPrivTCB -> HttpRequestHandler DC ())
loadApp appName = runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	_ <- setSessionDynFlags $ dflags { safeHaskell = Sf_Safe }
	target <- guessTarget appName Nothing
	addTarget target
	r <- load LoadAllTargets
	case r of
	  Failed -> error "Compilation failed"
	  Succeeded -> do
	    setContext [IIDecl $ simpleImportDecl (mkModuleName appName)]
	    value <- compileExpr (appName ++ ".server")
	    do let value' = (unsafeCoerce value) :: DCPrivTCB -> HttpRequestHandler DC ()
	       return value'

