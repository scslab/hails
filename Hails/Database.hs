{-# LANGUAGE Trustworthy, ScopedTypeVariables #-}

module Hails.Database ( mkPolicy, withDB ) where

import Data.Typeable
import LIO.MonadCatch
import LIO.DCLabel
import DCLabel.Core
import Data.String.Utils
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.DCAccess
import qualified Data.UString as U
import System.Environment
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as C

-- | Given a principal corresponding to the databaes owner and a
-- database name create the corresponding database object in @LIO@.
loadDatabase :: DatabasePolicy dbp
             => Principal
             -> DatabaseName
             -> (DC dbp)
loadDatabase dbPrincipal dbName = do
    let policyPriv = createPrivTCB $ newPriv dbPrincipal
    let dbConf = DBConf dbName policyPriv
    createDatabasePolicy dbConf

-- | Create a @DatabasePolicy@ with the appropriate underline databse
-- name and privileges, determined by the actual instance requested.
mkPolicy :: forall dbp. (DatabasePolicy dbp, Typeable dbp) => DC dbp
mkPolicy = do
  let tpc = undefined :: dbp
  let tp = typeRepTyCon $ typeOf $ tpc
  let typeName = tyConPackage tp ++ ":" ++ tyConModule tp ++
                  "." ++ tyConName tp
  maybe err doit $ lookup typeName databases
    where doit (dbName, dbPrincipal) = do
            result <- loadDatabase dbPrincipal dbName
            return $ result
          err = throwIO . userError $ "confLineToDBPair: could not parse line"

-- | Get the DB pair from a configuration line.
confLineToConfPair :: String
                   -> (String, (DatabaseName, Principal))
confLineToConfPair line = do
  case split "\t" line of
    (typeName:dbPrincipal:dbName:[]) -> (typeName, (dbN, dbP))
      where dbP = principal . C.pack $ dbPrincipal
            dbN = U.pack dbName
    _ -> ("",(undefined, undefined))

-- | Get all the databases in the system.
databases :: [(String, (DatabaseName, Principal))]
databases = unsafePerformIO $ do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap (split "\n") $ readFile configFile
  return $ map confLineToConfPair $ filter ((> 0) . length) confLines


-- | Given a database name and a database action, execute the action
-- on the database.
withDB :: DatabasePolicy dbp
       => dbp
       -> DCAction a
       -> DC (Either Failure a)
withDB dbp act = do
  let db = policyDB dbp
  dcAccess db act

