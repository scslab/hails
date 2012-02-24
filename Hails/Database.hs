{-# LANGUAGE Trustworthy #-}

module Hails.Database where

import LIO.MonadCatch
import LIO.DCLabel
import Data.String.Utils
import Hails.Utils.TCB
import Hails.Database.MongoDB (DCAction)
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.DCAccess (dcAccess)
import Database.MongoDB.Query (Failure)
import qualified Data.UString as U
import System.Environment
import System.IO.Unsafe

confLineToDBPair :: String
                 -> IO (String, DC (Database DCLabel))
confLineToDBPair line = do
  let (petName:dbName:policyMod:file:[]) = split ":" line
  db <- loadDatabase petName (U.pack dbName) policyMod file
  return (petName, db)

databases :: [(String, DC (Database DCLabel))]
databases = unsafePerformIO $ do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap (split "\n") $ readFile configFile
  mapM confLineToDBPair $ filter ((> 0) . length) confLines


withDB :: String
       -> DCAction a
       -> DC (Either Failure a)
withDB dbName act = do
  case lookup dbName databases of
    Just db' -> do
      db <- db'
      dcAccess db act
    Nothing -> throwIO NoSuchDatabase

