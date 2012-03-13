{-# LANGUAGE Trustworthy, ScopedTypeVariables #-}

module Hails.Database ( mkPolicy, withDB ) where

import Data.Typeable
import Data.IORef
import LIO.MonadCatch
import LIO.DCLabel
import LIO.TCB
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
  dbs <- ioTCB $ databases
  maybe err doit $ lookup typeName dbs
    where doit (dbName, dbPrincipal) = loadDatabase dbPrincipal dbName
          err = throwIO . userError $ "confLineToDBPair: could not parse line"

-- | Get the DB pair from a configuration line.
confLineToConfPair :: String
                   -> (String, (DatabaseName, Principal))
confLineToConfPair line = do
  case split "," line of
    (typeName:dbPrincipal:dbName:[]) -> (typeName, (dbN, dbP))
      where dbP = principal . C.pack $ dbPrincipal
            dbN = U.pack dbName
    _ -> ("",(undefined, undefined))

-- | Cache database specifications
databasesRef :: IO (IORef [(String, (DatabaseName, Principal))])
databasesRef = do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap lines $ readFile configFile
  newIORef $ map confLineToConfPair $ filter (not.null) confLines

-- | Get all the databases in the system.
databases :: IO [(String, (DatabaseName, Principal))]
databases = databasesRef >>= readIORef


-- | Given a database name and a database action, execute the action
-- on the database.
withDB :: DatabasePolicy dbp
       => dbp
       -> DCAction a
       -> DC (Either Failure a)
withDB dbp act = do
  let db = policyDB dbp
  dcAccess db act

