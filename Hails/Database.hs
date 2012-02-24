{-# LANGUAGE Trustworthy #-}

module Hails.Database ( withDB, databases ) where

import LIO.MonadCatch
import LIO.DCLabel
import Data.String.Utils
import Hails.Utils.TCB
import Hails.Database.MongoDB (DCAction)
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.DCAccess (dcAccess)
import Database.MongoDB.Query (Failure(..) )
import qualified Data.UString as U
import System.Environment
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as C

-- | Get the DB pair from a configuration line.
confLineToDBPair :: String
                 -> IO (DatabaseName, DC (Database DCLabel))
confLineToDBPair line = do
  case split ":" line of
    (dbPrincipal:dbName:policyMod:file:[]) -> do
      let dbP = principal . C.pack $ dbPrincipal
          dbN = U.pack dbName
      db <- loadDatabase dbP dbN policyMod file
      return (dbN, db)
    _ -> throwIO . userError $ "confLineToDBPair: could not parse line"

-- | Get all the databases in the system.
databases :: [(DatabaseName, DC (Database DCLabel))]
databases = unsafePerformIO $ do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap (split "\n") $ readFile configFile
  mapM confLineToDBPair $ filter ((> 0) . length) confLines


-- | Given a database name and a database action, execute the action
-- on the database.
withDB :: DatabaseName
       -> DCAction a
       -> DC (Either Failure a)
withDB dbName act = do
  maybe err doit $ lookup dbName databases
    where doit getDB = getDB >>= \db -> dcAccess db act
          err = return . Left $ QueryFailure (-1) "No such database"

