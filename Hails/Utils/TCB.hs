{-# LANGUAGE Unsafe #-}

module Hails.Utils.TCB ( loadDatabase
                       , loadApp 
                       ) where

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

import Data.IterIO.Http
import LIO.DCLabel
import DCLabel.Core

import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.DCAccess

-- | Given a principal corresponding to the databaes owner, a database
-- name, a policy module name, and  filepath to the database config file
-- create the corresponding database object in @LIO@.
loadDatabase :: Principal
             -> DatabaseName
             -> String
             -> FilePath
             -> IO (DC (Database DCLabel))
loadDatabase dbPrincipal dbName policyMod policyFile =
  runGhc (Just libdir) $ do
    let policyPriv = createPrivTCB $ newPriv dbPrincipal
    let dbConf = DBConf dbName policyPriv
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags $ dflags { safeHaskell = Sf_Safe }
    let target = Target (TargetFile policyFile Nothing) False Nothing
    addTarget target
    r <- load LoadAllTargets
    case r of
      Failed -> error "loadDatabase: Compilation failed"
      Succeeded -> do
        setContext [IIDecl $ simpleImportDecl (mkModuleName policyMod)]
        value <- fmap unsafeCoerce $ compileExpr "configDB"
        return $ value dbConf


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
      return $ ((unsafeCoerce value) :: DCPrivTCB -> HttpRequestHandler DC ())

