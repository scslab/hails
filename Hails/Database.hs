{-# LANGUAGE Trustworthy #-}

module Hails.Database where

import LIO
import Data.String.Utils
import Hails.Utils.TCB
import Hails.Database.MongoDB (Action)
import System.Environment
import System.IO.Unsafe

confLineToDBPair :: LabelState l p s
                 => String
                 -> IO (String, (Action l p s a -> Action l p s a))
confLineToDBPair line = do
  let (name:policyMod:file:[]) = split ":" line
  db <- loadDatabase name policyMod file
  return (name, db)

databases :: LabelState l p s
          => [(String, (Action l p s a -> Action l p s a))]
databases = unsafePerformIO $ do
  env <- getEnvironment
  let configFile = maybe "/etc/share/hails/conf/databases.conf" id
                      (lookup "DATABASE_CONFIG_FILE" env)
  confLines <- fmap (split "\n") $ readFile configFile
  mapM confLineToDBPair $ filter ((> 0) . length) confLines


withExternalDB :: LabelState l p s
               => String
               -> (Action l p s a -> Action l p s a)
withExternalDB name = let (Just result) = lookup name databases
  in result

