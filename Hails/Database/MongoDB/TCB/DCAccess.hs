module Hails.Database.MongoDB.TCB.DCAccess ( DBConf(..)
                                           , DCAction
                                           , dcAccess
                                           , labelDatabase
                                           ) where

import Hails.Database.MongoDB
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Access
import Database.MongoDB ( runIOE 
                        , connect
                        , host
                        , close
                        , master
                        , slaveOk
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

dcAccess :: Database DCLabel
         -> DCAction a
         -> DC (Either Failure a)
dcAccess db act = do
  env <- rtioTCB $ getEnvironment
  let hostName = maybe "localhost" id (List.lookup "HAILS_MONGODB_SERVER" env)
  let mode     = maybe master parseMode (List.lookup "HAILS_MONGODB_MODE" env)
  pipe <- rtioTCB $ runIOE $ connect (host hostName)
  accessTCB pipe mode db act

-- | Parse the access mode.
--
--  > slaveOk                : slaveOk
--  > unconfirmedWrites      : UnconfirmedWrites
--  > onfirmWrites <options> : ConfirmWrites [corresponding-options]
--  > _                      : master
--
-- where @options@ can be:
--
--  > fsync | journal | wait=<N>
--
-- separated by \';\', and @N@ is an integer.
-- Example: 
--
-- > HAILS_MONGODB_MODE = "confirmWrites wait=3;journal"
-- > HAILS_MONGODB_MODE = "master"
--
-- TODO (DS): add support for confirmWrites with getLastError.
--            The latter options:
--               fsync:true (wait for files to sync, when no journaling)
--               j:true     (wait for journal commit)
--               w:N        (wait for N writes)
parseMode :: String -> AccessMode
parseMode "slaveOk"           = slaveOk
parseMode "unconfirmedWrites" = UnconfirmedWrites
parseMode _                   = master

-- | The @withDB@ functions should use this function to label
-- their databases.
labelDatabase :: DBConf  -- ^ Database configuratoin
              -> DCLabel -- ^ Label of collection policies
              -> DCLabel -- ^ Database label
              -> DC (Database DCLabel)
labelDatabase conf lcoll lacc = do
  let name = dbConfName conf
      p    = dbConfPriv conf
  initColl <- labelP p lcoll Map.empty
  databaseP p name lacc initColl


