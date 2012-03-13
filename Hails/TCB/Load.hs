{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
module Hails.TCB.Load ( loadApp ) where

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

import Hails.TCB.Types ( AppName, AppReqHandler )


-- | Given an application name, return the corresponding computation.
loadApp :: AppName -> IO AppReqHandler
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
      return . unsafeCoerce $ value
