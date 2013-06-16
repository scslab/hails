{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts,
             ScopedTypeVariables #-}

{- |

A /policy module/ is a library with access to the privileges of a
dedicated principal (conceptually, the author of the library) and
associated with a dedicated database. The job of the policy module is
to specify what sort of data may be stored in this database, and what
access-control policies should be applied to it. However, because
Hails uses information flow control (IFC) to enforce policies, a
policy specified by a policy module on a piece of data is enforce even
when an app gets a hold of the data.

IFC lets /apps/ and policy modules productively use other policy
modules despite mutual distrust.  Moreover, IFC prevents malicious
apps from violating any of the policies specified by a policy module.
As a consequence, users need not place as much trust in apps. Rather,
they need to trust or verify the policies specified by policy modules.

This moule exports the class which every policy module must be an
instance of. Though simple, the class allows a policy module to create
collections with a set of policies and associate them with the policy
module's underlying database.

-}


module Hails.PolicyModule (
 -- * Creating policy modules
   PolicyModule(..)
 , PMAction
 -- ** Database and collection-set
 -- $db
 , labelDatabaseP
 , setDatabaseLabel, setDatabaseLabelP
 , setCollectionSetLabel, setCollectionSetLabelP
 -- * Collections
 -- $collection
 -- ** Creating collections
 , createCollection, createCollectionP
 -- ** Collection policies
 , CollectionPolicy(..), FieldPolicy(..)
 , isSearchableField
 , searchableFields
 -- * Using policy module databases
 -- $withPM
 , withPolicyModule 
 -- * Internal
 , TypeName
 , policyModuleTypeName
 , availablePolicyModules
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
import           LIO.TCB
import           LIO.DCLabel
import           Hails.Data.Hson
import           Hails.Database.Core
import           Hails.Database.TCB
import           Hails.PolicyModule.TCB

import           Text.Parsec hiding (label)

import           System.Environment
import           System.IO.Unsafe

-- | A policy module is specified as an instance of the @PolicyModule@
-- class. The role of this class is to define an entry point for
-- policy modules. The policy module author should set up the database
-- labels and create all the database collections in 'initPolicyModule'.
-- It is these collections and corresponding policies that apps and
-- other policy modules use when interacting with the policy module's
-- database using 'withPolicyModule'.
--
-- The Hails runtime system relies on the policy module's type @pm@ to
-- load the corresponding 'initPolicyModule' when some code \"invokes\"
-- the policy module using 'withPolicyModule'.  In fact when a piece of
-- code wishes to execute a database action on the policy module,
-- 'withPolicyModule' first executes the policy module's
-- 'initPolicyModule' and passes the result (of type @pm@) to the
-- invoking code.
--
-- Observe that 'initPolicyModule' has access to the policy module's
-- privileges, which are passed in as an argument.  This allows the
-- policy module to encapsulate its privileges in its @pm@ type and allow
-- code it trusts to use its privileges when executing a database action
-- using 'withPolicyModule'. Of course, untrusted code (which is usually
-- the case) should not be allow to inspect values of type @pm@ to get
-- the encapsulated privileges.
--
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
-- >      -- Label database and collection-set:
-- >      labelDatabaseP priv l l
-- >      -- Create collections:
-- >      createCollectionP priv "collection1" ...
-- >      createCollectionP priv "collection2" ...
-- >      ....
-- >      createCollectionP priv "collectionN" ...
-- >      -- Return the policy module:
-- >      return (MyPolicyModuleTCB priv)
--
-- Here the policy module labels the database, labels the list of
-- collections and finally creates @N@ collections.  The computation
-- returns a value of type @MyPolicyModule@ which wraps the policy
-- module's privileges. As a consequence, trustworthy code that has
-- access to the value constructor can use the policy module's
-- privileges:
--
-- > -- Trustworthy code within the same module (My.Policy)
-- >
-- > alwaysInsert doc = withPolicyModule $ \(MyPolicyModuleTCB priv) ->
-- >  insertP priv "collection1" doc
--
-- Here @alwaysInsert@ uses the policy module's privileges to insert a
-- document into collection \"collection1\". As such, if @doc@ is well-formed
-- the function always succeeds. (Of course, such functions should not be
-- exported.)
--
-- Untrusted code in a different module cannot, however use the policy
-- module's privilege:
--
-- > -- Untrusted code in a separate module
-- > import My.Policy
-- >
-- > maybeInsertIntoDB appPriv doc = withPolicyModule $ (_ :: MyPolicyModule) ->
-- >  insertP appPriv "collection1" doc
--
-- Depending on the privileges passed to @maybeInsertIntoDB@, and set
-- policies, the insertion may or may not succeed.
class Typeable pm => PolicyModule pm where
  -- | Entry point for policy module. Before executing the entry function,
  -- the current clearance is \"raised\" to the greatest lower bound of the
  -- current clearance and the label @\<\"Policy module principal\", |True\>@, 
  -- as to allow the policy module to read data labeled with its principal.
  initPolicyModule :: DCPriv -> PMAction pm

--
-- Setting database and collections labels
--

{- $db

The main role of a policy module is to provide a data model and
security policies on said data. To this end and as previously
mentioned, every policy module has access to a underlying MongoDB
database. Each database, in turn, has an associated collection-set: a
set of collections and their security policies.

At the coarsest level, the policy module can restrict access to the
database by labeling it with 'setDatabaseLabel'. By default, only the
policy module itself may access the database. Whenever an app or
another policy module accesses the database it is \"tainted\" by this
label. Strictly speaking, the database label is the label on the
database collection-set label. Hence, changing or observing the
collection-set label is directed by the database label.  Any
meaningful database action (e.g., insert, update, etc.) involves a
collection, and to observe the existence of collection in the database
requires reading the collection-set. As already noted, the
collection-set itself is protected by a label, which a policy module
sets with 'setCollectionSetLabel', which is used to taint code that
names collections of the database. Since the database label and
collection-set labels are closely related Hails allows policy modules
to set them using a single function 'labelDatabaseP'.

-}

-- | Set the label of the underlying database. The supplied label must
-- be bounded by the current label and clearance as enforced by
-- 'guardAlloc'. Moreover the current computation mut write to the
-- database, as enforce by applying 'guardWrite' to the current
-- database label. The latter requirement  suggests that every policy
-- module use 'setDatabaseLabelP' when first changing the label.
setDatabaseLabel :: DCLabel -> PMAction ()
setDatabaseLabel = setDatabaseLabelP noPriv

-- | Same as 'setDatabaseLabel', but uses privileges when performing
-- label comparisons. If a policy module wishes to allow other policy
-- modules or apps to access the underlying databse it must use 
-- @setDatabaseLabelP@ to \"downgrade\" the database label, which by
-- default only allows the policy module itself to access any of the
-- contents (including collection-set).
setDatabaseLabelP :: DCPriv    -- ^ Set of privileges
                  -> DCLabel   -- ^ New database label
                  -> PMAction ()
setDatabaseLabelP p l = liftDB $ do
  liftLIO $ guardAllocP p l
  db  <-  dbActionDB `liftM` getActionStateTCB
  liftLIO $ guardWriteP p (databaseLabel db)
  setDatabaseLabelTCB l

-- | The collections label protects the collection-set of the database.
-- It is used to restrict who can name a collection in the database and
-- who can modify the underlying collection-set (e.g., by creating a new
-- collection). The policy module may change the default collections
-- label, which limits access to the policy module alone, using
-- @setCollectionSetLabel@.
--
-- The new label must be bounded by the current label and clearance as
-- checked by 'guardAlloc'. Additionally, the current label must flow to
-- the label of the database which protects the label of the
-- colleciton set. In most cases code should use 'setCollectionSetLabelP'.
setCollectionSetLabel :: DCLabel -> PMAction ()
setCollectionSetLabel = setCollectionSetLabelP noPriv

-- | Same as 'setCollectionSetLabel', but uses the supplied privileges
-- when performing label comparisons.
setCollectionSetLabelP :: DCPriv      -- ^ Set of privileges
                       -> DCLabel     -- ^ New collections label
                       -> PMAction ()
setCollectionSetLabelP p l = liftDB $ do
  liftLIO $ guardAllocP p l
  db  <-  dbActionDB `liftM` getActionStateTCB
  liftLIO $ guardWriteP p (databaseLabel db)
  setCollectionSetLabelTCB l

-- | This is the first action that any policy module should execute.  It
-- is simply a wrapper for 'setDatabaseLabelP' and
-- 'setCollectionSetLabelP'.  Given the policy module's privilges, label
-- for the database, and label for the collection-set @labelDatabaseP@
-- accordingly sets the labels.
labelDatabaseP :: DCPriv    -- ^ Policy module privilges
               -> DCLabel   -- ^ Database label
               -> DCLabel   -- ^ Collections label
               -> PMAction ()
labelDatabaseP p ldb lcol = do
  setDatabaseLabelP p ldb
  setCollectionSetLabelP p lcol

--
-- Collections
--

{- $collection

As noted above a database consists of a set of collections. Each Hails
collection is a MongoDB collection with a set of labels and policies.
The main database \"work units\" are 'HsDocument's, which are stored
and retrieved from collections (see "Hails.Database"). 

Each collection has:

* A collection name, which is simply a string that can be used to name
  the collection. These names are protected by the collection-set
  label. Hence when performing @insert "myCollection" doc@, the name
  "myCollection" is always checked to actually be part of the
  databsae.

* A collection label. The collection label imposes a restriction on
  who can read and write to the collection.

* A collection clearance. The collection clearance imposes an upper
  bound on the sensitivity of data that can be stored in the
  collection.

* A collection policy (of type 'CollectionPolicy'). The collection
  policy specifies how collection documents and internal fields should
  be labeled . Specifically, a collection policy can be used to assign
  labeling policies to specific fields ('PolicyLabeled'), declares
  fields as 'SearchableField's (effectively readable by anybody that
  can read from the collection), and at a coarser level assign a label
  to each document.

The creation of collections, similar to setting database and
collection-set labels, is restricted to policy modules. Concretely, a
policy module may use 'createCollection' to create collections.

-}

-- | Create a 'Collection' given a name, label, clearance, and policy.
-- Several IFC rules must be respected for this function to succeed:
--
--  1. The supplied collection label and clearance must be above the
--     current label and below the current clearance as enforced by
--     'guardAlloc'.
--
-- 2. The current computation must be able to read the database
--    collection-set protected by the database label. The guard 'taint' is
--    used to guarantee this and raise the current label (to the
--    join of the current label and database label) appropriately.
-- 
-- 3. The computation must be able to modify the database collection-set.
--    The guard 'guardWrite' is used to guarantee that the current label
--    is essentially equal to the collection-set label.
--
-- Note: the collection policy is modified to make the @_id@ field
-- explicitly a 'SearchableField'.
createCollection :: CollectionName  -- ^ Collection name
                 -> DCLabel         -- ^ Collection label
                 -> DCLabel         -- ^ Collection clearance
                 -> CollectionPolicy-- ^ Collection policy
                 -> PMAction ()
createCollection = createCollectionP noPriv

-- | Same as 'createCollection', but uses privileges when performing
-- IFC checks.
createCollectionP :: DCPriv           -- ^ Privileges
                  -> CollectionName   -- ^ Collection name
                  -> DCLabel          -- ^ Collection label
                  -> DCLabel          -- ^ Collection clearance
                  -> CollectionPolicy -- ^ Collection policy
                  -> PMAction ()
createCollectionP p n l c pol = liftDB $ do
  db <- dbActionDB `liftM` getActionStateTCB
  liftLIO $ do
    taintP p $ databaseLabel db
    guardWriteP p $ labelOf (databaseCollections db)
    guardAllocP p l
    guardAllocP p c
  associateCollectionTCB $ collectionTCB n l c newPol
    where newPol = let ps  = fieldLabelPolicies pol
                       ps' = Map.insert (T.pack "_id") SearchableField ps
                   in pol { fieldLabelPolicies = ps' }

-- | Returns 'True' if the field policy is a 'SearchableField'.
isSearchableField :: FieldPolicy -> Bool
isSearchableField SearchableField = True
isSearchableField _ = False

-- | Get the list of names corresponding to 'SearchableField's.
searchableFields :: CollectionPolicy -> [FieldName]
searchableFields policy =
  Map.keys $ Map.filter isSearchableField fps
  where fps = fieldLabelPolicies policy

--
-- Managing databases
--

{- $withPM

Policy modules define a data model and security policies on the data.
Hence, in Hails, apps solely focus on implementing controllers and
viewers.  Apps may use different policy modules to implement a rich
experience without focusing on how to specify and enforce security
policies.  Moreover, Hails allows apps to use multiple policy modules
that may be in mutual distrust while guaranteeing that the policies of
each individual policy module are obeyed. Additionally, policy modules
may themselves rely on other policy modules to implement their duties.
A policy module's database is accessed using 'withPolicyModule'.

-}

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
-- > ("<Policy module package>:<Fully qualified module>.<Policy module type>", "<Policy module database name>")
-- 
-- Example of valid line is:
--
-- > ("my-policy-0.1.2.3:My.Policy.MyPolicyModule", "my_db")
--
-- The principal used by Hails is the first projection with a @\"_\"@
-- suffix. In the above, the principal assigned by Hails is:
--
-- > "_my-policy-0.1.2.3:My.Policy.MyPolicyModule"
availablePolicyModules :: Map TypeName (Principal, DatabaseName)
{-# NOINLINE availablePolicyModules #-}
availablePolicyModules = unsafePerformIO $ do
  conf <- getEnv "DATABASE_CONFIG_FILE"
  ls   <- lines `liftM` readFile conf
  Map.fromList `liftM` mapM xfmLine ls
    where xfmLine l = do (tn, dn) <- readIO l
                         return (tn,(principal (S8.pack $ '_':tn), dn))

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
      pipe <- ioTCB $ Mongo.runIOE $ Mongo.connect (Mongo.host hostName)
      let priv = MintTCB (toComponent pmOwner)
          s0 = makeDBActionStateTCB priv dbName pipe mode
      -- Execute policy module entry function with raised clearance:
      (policy, s1) <- withClearanceP' priv $ runDBAction (pmAct priv) s0
      let s2 = s1 { dbActionDB = dbActionDB s1 }
      res <- evalDBAction (act policy) s2
      ioTCB $ Mongo.close pipe
      return res
  where tn = policyModuleTypeName (undefined :: pm)
        pmAct priv = unPMActionTCB $ initPolicyModule priv :: DBAction pm
        withClearanceP' priv io = do
          c <- getClearance
          let lpriv = dcLabel (privDesc priv) (privDesc priv) `lub` c
          -- XXX: does this actually work? Used to be bracketP
          bracket
                   -- Raise clearance:
                   (setClearanceP priv lpriv)
                   -- Lower clearance:
                   (const $ do c' <- getClearance 
                               setClearanceP priv (partDowngradeP priv c' c))
                   -- Execute policy module entry point, in between:
                   (const io)

-- | Get the name of a policy module.
policyModuleTypeName :: PolicyModule pm => pm -> TypeName
policyModuleTypeName x =
   tyConPackage tp ++ ":" ++ tyConModule tp ++ "." ++ tyConName tp
      where tp = typeRepTyCon $ typeOf x
  
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

