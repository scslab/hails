module Hails.Database.MongoDB.TCB.DCAccess ( DBConf(..)
                                           , DCAction
                                           , dcAccess
                                           , labelDatabase
                                           ) where

import Hails.Database.MongoDB
import Hails.Database.MongoDB.TCB.Access
import Database.MongoDB (runIOE, connect, host, close, master)
import LIO
import LIO.TCB ( rtioTCB )
import LIO.DCLabel

import qualified Data.Map as Map

-- | Database configuration, used to invoke @withDB@
data DBConf = DBConf { dbConfName :: DatabaseName
                     , dbConfPriv :: DCPrivTCB
                     } deriving (Show)

type DCAction = Action DCLabel DCPrivTCB ()

dcAccess :: Database DCLabel
         -> DCAction a
         -> DC (Either Failure a)
dcAccess db act = do
  --TODO: get from ENV
  pipe <- rtioTCB $ runIOE $ connect(host "127.0.0.1")
  let mode = master
  accessTCB pipe mode db act

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
