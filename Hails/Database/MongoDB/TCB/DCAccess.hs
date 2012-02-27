{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
module Hails.Database.MongoDB.TCB.DCAccess ( DBConf(..)
                                           , DCAction
                                           , dcAccess
                                           , labelDatabase
                                           ) where

import Data.Bson (u)
import qualified Data.Bson as Bson
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Access
import Database.MongoDB.Query (Failure)
import Database.MongoDB ( runIOE
                        , connect
                        , host
                        , master
                        , slaveOk
                        , GetLastError
                        , AccessMode(..) )
import LIO
import LIO.TCB ( rtioTCB )
import LIO.DCLabel
import System.Environment

import qualified Data.Map as Map
import qualified Data.List as List

import Text.Parsec

-- | Database configuration, used to invoke @withDB@
data DBConf = DBConf { dbConfName :: DatabaseName
                     , dbConfPriv :: DCPrivTCB
                     } deriving (Show)

type DCAction = Action DCLabel DCPrivTCB ()

-- | Open a pipe to a supplied server, or @localhost@.
-- TODO: add support for connecting to replicas.
dcAccess :: Database DCLabel
         -> DCAction a
         -> DC (Either Failure a)
dcAccess db act = do
  env <- rtioTCB $ getEnvironment
  let hostName = maybe "localhost" id (List.lookup "HAILS_MONGODB_SERVER" env)
  let mode     = maybe master parseMode (List.lookup "HAILS_MONGODB_MODE" env)
  pipe <- rtioTCB $ runIOE $ connect (host hostName)
  accessTCB pipe mode db act


-- | The @withDB@ functions should use this function to label
-- their databases.
-- TODO (DS/AL(: make every searchable field indexable.
labelDatabase :: DBConf  -- ^ Database configuratoin
              -> DCLabel -- ^ Label of collection policies
              -> DCLabel -- ^ Database label
              -> DC (Database DCLabel)
labelDatabase conf lcoll lacc = do
  let dbName = dbConfName conf
      p    = dbConfPriv conf
  initColl <- labelP p lcoll Map.empty
  databaseP p dbName lacc initColl

--
-- Parser for getLastError
--


-- | Parse the access mode.
--
--  > slaveOk                : slaveOk
--  > unconfirmedWrites      : UnconfirmedWrites
--  > onfirmWrites <options> : ConfirmWrites [corresponding-options]
--  > _                      : master
--
-- where @options@ can be:
--
--  > fsync | journal | writes=<N>
--
-- separated by \',\', and @N@ is an integer.
-- Example: 
--
-- > HAILS_MONGODB_MODE = "slaveOk"
-- > HAILS_MONGODB_MODE = "confirmWrites: writes=3, journal"
-- > HAILS_MONGODB_MODE = "master"
--
parseMode :: String -> AccessMode
parseMode "slaveOk"           = slaveOk
parseMode "unconfirmedWrites" = UnconfirmedWrites
parseMode xs = case parse wParser "" xs of
                 Right le -> ConfirmWrites le
                 Left _ -> master
  where wParser = do _ <- string "confirmWrites" 
                     spaces
                     _ <- char ':'
                     spaces
                     gle_opts

gle_opts :: Stream s m Char => ParsecT s u m GetLastError
gle_opts = do opt_first <- gle_opt
              opt_rest  <- gle_opts'
              return $ opt_first ++ opt_rest
    where gle_opt = gle_opt_fsync <|> gle_opt_journal <|> gle_opt_write   
          gle_opts' :: Stream s m Char => ParsecT s u m GetLastError
          gle_opts' = (spaces >> char ',' >> spaces >> gle_opts) <|> (return [])

gle_opt_fsync :: Stream s m Char => ParsecT s u m GetLastError
gle_opt_fsync = string "fsync" >> return [ (u "fsync") Bson.=: True ]

gle_opt_journal :: Stream s m Char => ParsecT s u m GetLastError
gle_opt_journal = string "journal" >> return [ (u "j") Bson.=: True ]

gle_opt_write :: Stream s m Char => ParsecT s u m GetLastError
gle_opt_write   = do _ <- string "write"
                     spaces
                     _ <- char '='
                     spaces
                     dgt <- many1 digit
                     return [ (u "w") Bson.=: (read dgt :: Integer) ]

