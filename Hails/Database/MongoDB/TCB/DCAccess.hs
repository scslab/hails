{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hails.Database.MongoDB.TCB.DCAccess ( DBConf(..)
                                           , DCAction
                                           , dcAccess
                                           , labelDatabase
                                           , DatabasePolicy(..)
                                           -- * Groups
                                           , PolicyGroup(..)
                                           , relabelGroupsP
                                           , relabelGroupsSafe
                                           -- * Privilege granting gate
                                           , PrivilegeGrantGate(..)
                                           ) where

import Control.Monad (foldM, liftM)
import Data.Bson (u)
import qualified Data.Bson as Bson
import Hails.Database.MongoDB.TCB.Types
import Hails.Database.MongoDB.TCB.Access
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

import Data.Maybe (fromMaybe)
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
  env <- rtioTCB getEnvironment
  let hostName = fromMaybe "localhost" (List.lookup "HAILS_MONGODB_SERVER" env)
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

-- | Policy modules are instances of this class. In particular, when
-- an application accesses a database, the runtime invokes
-- @createDatabasePolicy@ in the appropriate policy module.
class DatabasePolicy dbp where
  -- | Given a 'DBConf' generate an instance of this
  -- @DatabasePolicy@. This is the main entry point for policy
  -- modules. Policies should, in general, ether discard @DBConf@ or
  -- store it in such a way that it is inaccessible to other modules
  -- since it contains the priviledge of the policy.
  createDatabasePolicy :: DBConf -> DCPrivTCB -> DC dbp

  -- | Get the actual underlying @Database@ instance for this policy.
  policyDB :: dbp -> Database DCLabel

  -- | Get the database policy's owner.
  policyOwner :: dbp -> Principal

-- | Class used to define groups in a policy-specific manner.
class DatabasePolicy dbp => PolicyGroup dbp where
  -- | Expands a principal of the form \"#group_name\" into a list of
  -- @Principal@s
  expandGroup :: dbp -> Principal -> DCAction [Principal]
  expandGroup _ princ = return [princ]

  -- | Relabeles the 'Labeled' value by using the policy's privilege
  -- to downgrade the label and optionally re-taint in an application
  -- specific way, e.g. exanding groups of the form \"#group_name\"
  -- to a policy specified disjuction of real principals.
  --
  -- Policies are expected to implement this function by wrapping
  -- 'relabelGroupsP' using their privilege and implementing
  -- 'expandGroup', which is called by 'relabelGroupsP'.
  relabelGroups :: dbp -> DCLabeled a -> DC (DCLabeled a)
  relabelGroups _ = return


-- | Class used to define policy-specifi privilege granting gate.
class DatabasePolicy dbp => PrivilegeGrantGate dbp where
  -- | Request the policy's privilege-granting gate.
  grantPriv :: dbp        -- ^ Policy
            -> Principal  -- ^ App principal
            -> DC (DCGate DCPrivTCB)

-- | A wrapper around 'relabelGroups' that drops the current
-- privileges and restores them after getting a result from
-- 'relabelGroups'.
relabelGroupsSafe :: PolicyGroup dbp
                  => dbp
                  -> Labeled DCLabel a
                  -> DC (DCLabeled a)
relabelGroupsSafe dbp lbl = withPrivileges noPrivs $
  relabelGroups dbp lbl

-- | Looks for disjuctions the privilege is able to downgrade and
-- rewrites them by invoking 'expandGroup' on each principle in the
-- disjuction. Using the result, the function relabels the 'Labeled'
-- value. Clients should not call this directly, instead clients
-- should call 'relabelGroups' which policies may implement by
-- wrapping this function.
relabelGroupsP :: PolicyGroup dbp
               => dbp
               -> DCPrivTCB
               -> Labeled DCLabel a
               -> DC (DCLabeled a)
relabelGroupsP dbp p inp = do
  let (MkDCLabel sec' inte') = labelOf inp
  sec <- expandComponent sec'
  inte <- expandComponent inte'
  let lbl = MkDCLabel sec inte
  relabelP p lbl inp
  where expandComponent l | l == (><)  = return l
        expandComponent comp = do
          ds <- mapM gocmp $ componentToList comp
          return $ listToComponent ds
        gocmp d = do
          let db = policyDB dbp
          result <-
            if p `owns` d
              then dcAccess db $ liftM listToDisj $
                foldM (\res grp -> do next <- expandGroup dbp grp
                                      return $ res ++ next) [] $ disjToList d
              else return $ Right d
          return $ case result of
            Right dr -> dr
            Left _ -> d

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

