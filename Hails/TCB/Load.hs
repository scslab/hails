{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
module Hails.TCB.Load ( loadApp ) where

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce
import Control.Monad (void)

import Hails.TCB.Types ( AppName, AppReqHandler )


-- | Given an application name, return the corresponding computation.
loadApp :: Bool -> AppName -> IO AppReqHandler
loadApp safe appName = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflagsXSafe = if safe
                      then dflags { safeHaskell = Sf_Safe }
                      else dflags
  void $ setSessionDynFlags dflagsXSafe
  target <- guessTarget appName Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
    Failed -> error "Compilation failed"
    Succeeded -> do
      setContext [IIDecl $ simpleImportDecl (mkModuleName appName)]
      value <- compileExpr (appName ++ ".server") 
      return . unsafeCoerce $ value
