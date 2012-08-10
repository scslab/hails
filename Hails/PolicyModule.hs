{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts,
             ScopedTypeVariables #-}

{- |

A /policy module/ is a library with access to the privileges of a
dedicated principal (conceptually, the author of the library) and
associated with a dedicated database ('PolicyModuleConf'). The job of
the policy module is to specify what sort of data may be stored in
this database, and what access-control policies should be applied to
it. However, because Hails uses information flow control (IFC) to enforce
policies, a policy specified by a policy module on a piece of data is
enforce even when an app gets a hold of the data.

IFC lets apps and policy modules productively use other policy modules
despite mutual distrust.  Moreover, IFC prevents malicious apps from
violating any of the policies specified by a policy module. As a
consequence, users need not place as much trust in apps. Rather, they
need to trust or verify the policies specified by policy modules.

This moule exports the class which every policy module must be an
instance of. Though simple, the class allows a policy module to create
a database and relevant collections with a set of policies.

-}


module Hails.PolicyModule (
  -- * Creating policy modules
   PolicyModule(..)
 -- ** Helpers
 , labelDatabaseP
 , associateCollectionsP 
 -- * Using policy module databases
 , withPolicyModule 
 -- * Internal
 , availablePolicyModules, TypeName
 ) where


import           Data.Maybe
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.Bson as Bson

import           Control.Monad

import qualified Database.MongoDB as Mongo
import           Database.MongoDB (GetLastError)

import           LIO
import           LIO.Privs.TCB (mintTCB)
import           LIO.TCB (ioTCB, rethrowIoTCB)
import           LIO.DCLabel
import           Hails.Database.Core
import           Hails.Database.TCB (makeDBActionStateTCB)
import           Hails.Database.Query

import           Text.Parsec hiding (label)

import           System.Environment
import           System.IO.Unsafe

-- | A policy module is specified as an instance of the @PolicyModule@
-- class. The role of this class is to define an entry point for
-- policy modules. The policy module author should set up the database
-- labels and define all the database collections in 'initPolicyModule'.
-- It is these collections and corresponding policies that apps and
-- other policy modules use when interacting with the policy module's
-- database.
--
-- The Hails runtime system relies on the policy module's type @pm@ to
-- load the corresponding 'initPolicyModule' when some code \"invokes\"
-- the policy module using 'withPolicyModule'.  In fact when a piece of
-- code wishes to execute a database action on the policy module,
-- 'withPolicyModule' first executes the policy modules
-- 'initPolicyModule' and passes the result (of type @pm@) to the
-- invoking code. Observe that 'initPolicyModule' has access to the
-- policy module's privileges, which are passed in as an argument.
-- This allows the policy module to encapsulate its privileges in its
-- @pm@ type and allow code it trusts to use its privileges when
-- executing a database action using 'withPolicyModule'. Of course, 
-- untrusted code (which is usually the case) should not be allow to
-- inspect values of type @pm@ to get the encapsulated privileges.
-- Consider the example below:
--
-- >  module My.Policy ( MyPolicyModule ) where
-- >
-- >  import LIO
-- >  import LIO.DCLabel
-- >  import Data.Typeable
-- >  import Hails.PolicyModule
-- >  
-- >  -- | Handle to policy module, not exporting @MyPolicyModuleTCB@
-- >  data MyPolicyModule = MyPolicyModuleTCB DCPriv deriving Typeable
-- >  
-- >  instance PolicyModule MyPolicyModule where
-- >    initPolicyModule priv = do
-- >          -- Get the policy module principal:
-- >      let this = privDesc priv
-- >          -- Create label:
-- >          l    = dcLabel dcTrue -- Everybody can read
-- >                         this   -- Only policy module can modify
-- >      -- Label database and collection map:
-- >      labelDatabaseP priv l l
-- >      -- Associate collections with database
-- >      associateCollectionsP priv [ {- ... my collections ... -} ]
-- >      -- Return "handle" to this policy jodule
-- >      return (MyPolicyModuleTCB priv)
--
-- Here the policy module labels the database, labels the list of
-- collections and finally associates its collections with the
-- database. The computation returns a value of type @MyPolicyModule@
-- which wraps the policy module's privileges. As a consequence,
-- trustworthy code that has access to the value constructor can use
-- the policy module's privileges:
--
-- > -- Trustworthy code within the same module (My.Policy)
-- >
-- > alwaysInsert doc = withPolicyModule $ \(MyPolicyModuleTCB priv) ->
-- >  insertP priv doc
--
-- Here @alwaysInsert@ uses the policy module's privileges to insert a
-- document into the database. As such, if @doc@ is well-formed the
-- function always succeeds. (Of course, such functions should not be
-- exported.)
--
-- Untrusted code in a different module cannot, however use the policy
-- module's privilege:
--
-- > -- Untrusted code in a separate module
-- > import My.Policy
-- >
-- > maybeInsertIntoDB appPriv doc = withPolicyModule $ (_ :: MyPolicyModule) ->
-- >  insertP appPriv doc
--
-- Depending on the privileges passed to @maybeInsertIntoDB@, the
-- insertion may or may not succeed.
class Typeable pm => PolicyModule pm where
  -- | Entry point for policy module. Before executing the entry function,
  -- the current clearance is \"raised\" to the greatest lower bound of the
  -- current clearance and the label @<"Policy module principal", |True>@, 
  -- as to allow the policy module to read data labeled with its principal.
  initPolicyModule :: DCPriv -> DBAction pm

-- | This is the first action that any policy module should execute.
-- Given the policy module's privilges, label for the databse, and
-- label for the collection map @labelDatabaseP@ accordingly sets the
-- labels on the policy module's database.
labelDatabaseP :: DCPriv    -- ^ Policy module privilges
               -> DCLabel   -- ^ Database label
               -> DCLabel   -- ^ Collections label
               -> DBAction ()
labelDatabaseP p ldb lcol = do
  setDatabaseLabelP p ldb
  setCollectionsLabelP p lcol

-- | Given the policy module's privileges and list of collections,
-- associate the collections with the policy module's database.
associateCollectionsP :: DCPriv         -- ^ Policy module privileges
                      -> [Collection]   -- ^ List of collections
                      -> DBAction ()
associateCollectionsP p cs = mapM_ (associateCollectionP p) cs

--
-- Managing databases
--

-- | Policy type name. Has the form:
--
-- > <Policy module package>:<Fully qualified module>.<Policy module type>
type TypeName = String

-- | This contains a map of all the policy modules. Specifically, it
-- maps the policy moule types to a pair of the policy module
-- principal and database name.
--
-- /For the trusted programmer:/
-- The map itself is read from the file pointed to by the environment
-- variable @DATABASE_CONFIG_FILE@. Each line in the file corresponds
-- to a policy module. The format of a line is as follows
--
-- > ("<Policy module package>:<Fully qualified module>.<Policy module type>", "<Policy module principal>", "<Policy module database name>")
-- 
-- Example of valid line is:
--
-- > ("my-policy-0.1.2.3:My.Policy.MyPolicyModule","_my_policy_module","my_db")
--
availablePolicyModules :: Map TypeName (Principal, DatabaseName)
{-# NOINLINE availablePolicyModules #-}
availablePolicyModules = unsafePerformIO $ do
  conf <- getEnv "DATABASE_CONFIG_FILE"
  ls   <- lines `liftM` readFile conf
  Map.fromList `liftM` mapM xfmLine ls
    where xfmLine l = do (tn, p, dn) <- readIO l
                         return (tn,(principal (S8.pack p), dn))

-- | This function is the used to execute database queries on policy
-- module databases. The function firstly invokes the policy module,
-- determined from the type @pm@, and creates a pipe to the policy
-- module's database. The supplied database query function is then
-- applied to the policy module. In most cases, the value of type @pm@ is
-- opaque and the query is executed without additionally privileges.
--
-- > withPolicyModule $ \(_ :: SomePolicyModule) -> do
-- >  -- Perform database operations: insert, save, find, delete, etc.
--
-- Trustworthy code (as deemed by the policy module) may, however, be
-- passed in additional privileges by encapsulating them in @pm@ (see
-- 'PolicyModule').
withPolicyModule :: forall a pm. PolicyModule pm => (pm -> DBAction a) -> DC a
withPolicyModule act = do
  case Map.lookup tn availablePolicyModules of
    Nothing -> throwLIO UnknownPolicyModule 
    Just (pmOwner, dbName) -> do
      env <- ioTCB $ getEnvironment
      let hostName = fromMaybe "localhost" $
                               List.lookup "HAILS_MONGODB_SERVER" env
          mode     = maybe master parseMode $
                                  List.lookup "HAILS_MONGODB_MODE" env
      pipe <- rethrowIoTCB $ Mongo.runIOE $ Mongo.connect (Mongo.host hostName)
      let priv = mintTCB (toComponent pmOwner)
          s0 = makeDBActionStateTCB priv dbName pipe mode
      (policy, s1) <- runDBAction (initPolicyModule' priv) s0
      evalDBAction (act policy) s1 { dbActionDB = dbActionDB s1 }
  where tp = typeRepTyCon $ typeOf $ (undefined :: pm)
        tn = tyConPackage tp ++ ":" ++ tyConModule tp ++ "." ++ tyConName tp
        initPolicyModule' priv = do
          c <- getClearance
          let lpriv = dcLabel (privDesc priv) dcTrue
          bracketP priv
                   -- Raise clearance:
                   (setClearanceP priv $ partDowngradeP priv c lpriv)
                   -- Lower clearance:
                   (const $ do c' <- getClearance 
                               setClearanceP priv (partDowngradeP priv c' c))
                   -- Execute policy module entry point, in between:
                   (const (initPolicyModule priv :: DBAction pm))

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
gle_opt_fsync = string "fsync" >> return [ (T.pack "fsync") Bson.=: True ]

gle_opt_journal :: Stream s m Char => ParsecT s u m GetLastError
gle_opt_journal = string "journal" >> return [ (T.pack "j") Bson.=: True ]

gle_opt_write :: Stream s m Char => ParsecT s u m GetLastError
gle_opt_write   = do _ <- string "write"
                     spaces
                     _ <- char '='
                     spaces
                     dgt <- many1 digit
                     return [ (T.pack "w") Bson.=: (read dgt :: Integer) ]

