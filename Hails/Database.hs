{-# LANGUAGE Trustworthy, ScopedTypeVariables #-}

module Hails.Database ( mkPolicy, withDB ) where

import Data.Typeable
import LIO.MonadCatch
import LIO.DCLabel
import LIO.TCB
import DCLabel.Core
import Data.String.Utils
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.DCAccess
import qualified Data.UString as U
import System.Environment
import qualified Data.ByteString.Char8 as C

-- | Given a principal corresponding to the databaes owner and a
-- database name create the corresponding database object in @LIO@.
loadDatabase :: DatabasePolicy dbp => Principal
             -> DatabaseName
             -> (DC dbp)
loadDatabase dbPrincipal dbName = do
    let policyPriv = createPrivTCB $ newPriv dbPrincipal
    let dbConf = DBConf dbName policyPriv
    clr <- getClearance
    lowerClrTCB $ newDC dbPrincipal (<>)
    res <- createDatabasePolicy dbConf policyPriv
    lowerClrTCB clr
    return res

-- | Create a @DatabasePolicy@ with the appropriate underline databse
-- name and privileges, determined by the actual instance requested.
mkPolicy :: forall dbp. (DatabasePolicy dbp, Typeable dbp) => DC dbp
mkPolicy = do
  let tp = typeRepTyCon $ typeOf $ (undefined :: dbp)
  let typeName = tyConPackage tp ++ ":" ++ tyConModule tp ++
                  "." ++ tyConName tp
  dbs <- ioTCB $ databases
  maybe (err typeName) doit $ lookup typeName dbs
    where doit (dbName, dbPrincipal) = loadDatabase dbPrincipal dbName
          err tn = throwIO . userError $ "mkPolicy: could not find policy for "
                                          ++ tn

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
databases :: IO [(String, (DatabaseName, Principal))]
databases = do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap lines $ readFile configFile
  return $ map confLineToConfPair $ filter (not.null) confLines

-- | Given a database name and a database action, execute the action
-- on the database.
withDB :: DatabasePolicy dbp
       => dbp
       -> DCAction a
       -> DC (Either Failure a)
withDB dbp act = do
  let db = policyDB dbp
  dcAccess db act

