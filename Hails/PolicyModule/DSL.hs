{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TypeSynonymInstances,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}
{- |

This module exports a domain specific language for specifying policy
module policies. It is recommended that /all/ policy modules use this
code when specifying security policies as it simplifies auditing and
building trust in the authors. Policy modules are described in
"Hails.PolicyModule", which is a pre-required reading to this
module\'s documentation.

Consider creating a policy module where anybody can read and write
freely to the databse. In this databsae we wish to create a simple
user model collecting user names and passwords. This collection
\"users\" is also readable and writable by anybody. However, the
passwords must always belong to the named user. Specifically, only the
user (or policy module) may read and modify the password. This policy
is implemented below:

@
data UsersPolicyModule = UsersPolicyModuleTCB DCPriv
  deriving Typeable

instance PolicyModule UsersPolicyModule where
  'initPolicyModule' priv = do
    'setPolicy' priv $ do
      'database' $ do
        'readers' '==>' 'anybody'
        'writers' '==>' 'anybody'
        'admins'  '==>' this
      'collection' \"users\" $ do
        'access' $ do
          'readers' '==>' 'anybody'
          'writers' '==>' 'anybody'
        'clearance' $ do
          'secrecy'   '==>' this
          'integrity' '==>' 'anybody'
        'document' $ \doc -> do
          'readers' '==>' 'anybody'
          'writers' '==>' 'anybody'
        'field' \"name\"     $ 'searchable'
        'field' \"password\" $ 'labeled' $ \doc -> do
          let user = \"name\" ``at`` doc :: String
          'readers' '==>' this \\/ user
          'writers' '==>' this \\/ user
    return $ UsersPolicyModuleTCB priv
      where this = 'privDesc' priv
@


Notice that the database is public, as described above, but only this
policy module may modify the internal collection names (as indicated
by the 'admin' keyword). Similarly the collection is publicly
accessible (as set with the 'access' keyword), and may contain data at
most as sensitve as the policy module can read (i.e., the
'clearance').

Documents retrieved from the \"users\" 'collection' are public (as
indicated by the 'document' data-dependent policy that sets the
'readers' and 'writers'). The 'field' \"name\" is 'searchable' (i.e.,
it is a 'SearchableField') and thus can be used in query predicates.
Conversely, the \"password\" 'field' is 'labeled' using a
data-dependent policy. Specifically the field is labed using the
\"name\" value contained in the document (i.e., the user\'s name):
hence only the user having the right privilege or the policy module
(@this@) may read and create such data.

-}
 

module Hails.PolicyModule.DSL (
    setPolicy
  -- * Label components (or roles)
  , readers, secrecy
  , writers, integrity
  , admins
  , (==>), (<==)
  -- * Creating databases label policies
  , database
  -- * Creating collection policies
  , collection
  , access
  , clearance
  , document
  -- * Creating field policies
  , field, searchable, key, labeled
  ) where

import           Data.Maybe
import           Data.List (isPrefixOf)
import           Data.Map (Map)
import           Data.Traversable (forM)
import           Data.Typeable
import qualified Data.Map as Map
import qualified Data.Text as T
import           Control.Monad hiding (forM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader hiding (ask)
import           Control.Monad.Trans.State hiding (put, get)
import           Control.Monad.Trans.Error
import           Control.Monad.State.Class
import           Control.Monad.Reader.Class
import           Control.Exception
import           LIO
import           LIO.DCLabel
import           Hails.PolicyModule
import           Hails.Database

-- | Type denoting readers.
data Readers = Readers
instance Show Readers where show _ = "readers"

-- | Set secrecy component of the label, i.e., the principals that can
-- read.
readers, secrecy :: Readers
readers =  Readers
secrecy =  Readers

-- | Type denoting writers.
data Writers = Writers
instance Show Writers where show  _ = "writers"

-- | Set integrity component of the label, i.e., the principals that can
-- write.
writers, integrity :: Writers 
writers =  Writers 
integrity =  Writers 

-- | Used when setting integrity component of the collection-set label, i.e.,
-- the principals/administrators that can modify a database's underlying
-- collections.
data Admins  = Admins
instance Show Admins where show _ = "admins"

-- | Synonym for 'Admins'.
admins :: Admins 
admins =  Admins 

infixl 5 ==>, <==

-- | Class used for creating micro policies.
class MonadState s m => Role r s m where
  -- | @r ==> c@ effectively states that role @r@ (i.e., 'readers',
  -- 'writers', 'admins' must imply label component @c@).
  (==>) :: (ToComponent c) => r -> c -> m ()
  -- | Inverse implication. Purely provided for readability. The
  -- direction is not relevant to the internal representation.
  (<==) :: (ToComponent c) => r -> c -> m ()
  (<==) = (==>)


--
--
--

-- | Type representing a database expression. 
--
-- > database $ do
-- >   readers ==> "Alice" \/ "Bob"
-- >   writers ==> "Alice"
-- >   admins  ==> "Alice"
--
data DBExp = DBExp Component Component Component
  deriving Show

-- | Database expression solely contains a list of components.
type DBExpS = Map String Component

-- | Database expression composition monad
newtype DBExpM a = DBExpM (ErrorT String (State DBExpS) a)
  deriving (Monad, MonadState DBExpS)

instance Role Readers DBExpS DBExpM where 
  _ ==> c = DBExpM $ do
    s <- get 
    case Map.lookup (show readers) s of
      Just _ -> fail "Database readers already specified."
      Nothing -> put $ Map.insert (show readers) (toComponent c) s

instance Role Writers DBExpS DBExpM where 
  _ ==> c = DBExpM $ do
    s <- get 
    case Map.lookup (show writers) s of
      Just _ -> fail "Database writers already specified."
      Nothing -> put $ Map.insert (show writers) (toComponent c) s

instance Role Admins DBExpS DBExpM where 
  _ ==> c = DBExpM $ do
    s <- get 
    case Map.lookup (show admins) s of
      Just _ -> fail "Database admins already specified."
      Nothing -> put $ Map.insert (show admins) (toComponent c) s


-- | Create a database lebeling policy The policy must set the label
-- of the database, i.e., the 'readers' and 'writers'. Additionally it
-- must state the 'admins' that can modify the underlying collection-set 
--
-- For example, the policy
-- 
-- > database $ do
-- >   readers ==> "Alice" \/ "Bob" \/ "Clarice"
-- >   writers ==> "Alice" \/ "Bob"
-- >   admins  ==> "Alice"
--
-- states that Alice, Bob, and Clarice can read from the database,
-- including the collections in the database (the 'readers' is used as
-- the secrecy component in the collection-set label). Only Alice or
-- Bob may, however, write to the database. Finally, only Alice can
-- add additional collections in the policy module code.
--
database :: DBExpM () -> PolicyExpM ()
database (DBExpM e) = do
  s <- get
  case Map.lookup "database" s of
    Just _ -> fail "Database labels already set"
    Nothing -> case evalState (runErrorT e') Map.empty of
                 Left err -> fail err
                 Right dbExp -> put $ Map.insert "database"
                                                 (PolicyDBExpT dbExp) s
    where e' = do e
                  s <- get
                  r <- lookup' (show readers) s
                  w <- lookup' (show writers) s
                  a <- lookup' (show admins ) s
                  return $ DBExp r w a
          lookup' k s = maybe (fail $ "Missing " ++ show k)
                            return $ Map.lookup k s

--------------------------------------------------------------

-- | Type representing a collection access label expression.
--
-- > access $ do
-- >   readers ==> "Alice" \/ "Bob"
-- >   writers ==> "Alice"
--
data ColAccExp = ColAccExp Component Component
  deriving Show

-- | Access expression solely contains a list of components.
type ColAccExpS = Map String Component

-- | Access expression composition monad
newtype ColAccExpM a =
  ColAccExpM (ErrorT String (StateT ColAccExpS (Reader CollectionName)) a)
  deriving (Monad, MonadState ColAccExpS, MonadReader CollectionName)

instance Role Readers ColAccExpS ColAccExpM where 
  _ ==> c = ColAccExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show readers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " access readers already specified."
      Nothing -> put $ Map.insert (show readers) (toComponent c) s

instance Role Writers ColAccExpS ColAccExpM where 
  _ ==> c = ColAccExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show writers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " access writers already specified."
      Nothing -> put $ Map.insert (show writers) (toComponent c) s


--------------------------------------------------------------

-- | Type representing a collection clearance label expression.
--
-- > clearance $ do
-- >   readers ==> "Alice" \/ "Bob"
-- >   writers ==> "Alice"
--
data ColClrExp = ColClrExp Component Component
  deriving Show

-- | Clress expression solely contains a list of components.
type ColClrExpS = Map String Component

-- | Database expression composition monad
newtype ColClrExpM a =
  ColClrExpM (ErrorT String (StateT ColClrExpS (Reader CollectionName)) a)
  deriving (Monad, MonadState ColClrExpS, MonadReader CollectionName)

instance Role Readers ColClrExpS ColClrExpM where 
  _ ==> c = ColClrExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show readers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " clearance readers already specified."
      Nothing -> lift . put $ Map.insert (show readers) (toComponent c) s

instance Role Writers ColClrExpS ColClrExpM where 
  _ ==> c = ColClrExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show writers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " clearance writers already specified."
      Nothing -> put $ Map.insert (show writers) (toComponent c) s



--------------------------------------------------------------

-- | Type representing a collection document label expression.
--
-- > document $ \doc -> do
-- >   readers ==> "Alice" \/ "Bob"
-- >   writers ==> "Alice"
--
data ColDocExp = ColDocExp (HsonDocument -> LabelExp)
instance Show ColDocExp where show _ = "ColDocExp {- function -}"

-- | A Label expression has two components.
data LabelExp = LabelExp Component Component

-- | Document expression solely contains a list of components.
type ColDocExpS = Map String Component

-- | Document expression composition monad
newtype ColDocExpM a =
  ColDocExpM (ErrorT String (StateT ColDocExpS (Reader CollectionName)) a)
  deriving (Monad, MonadState ColDocExpS, MonadReader CollectionName)

instance Role Readers ColDocExpS ColDocExpM where 
  _ ==> c = ColDocExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show readers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " document readers already specified."
      Nothing -> lift . put $ Map.insert (show readers) (toComponent c) s

instance Role Writers ColDocExpS ColDocExpM where 
  _ ==> c = ColDocExpM $ do
    s <- get 
    cName <- ask
    case Map.lookup (show writers) s of
      Just _ -> fail $ "Collection " ++ show cName
                          ++ " document writers already specified."
      Nothing -> put $ Map.insert (show writers) (toComponent c) s



--------------------------------------------------------------

-- | Type representing a collection field policy expression.
--
-- > field "name"  searchable
-- > field "password" $ labeled $ \doc -> do
-- >   readers ==> (((T.pack "name") `at`doc) :: String)
-- >   writers ==> (((T.pack "name") `at`doc) :: String)
--
data ColFieldExp = ColFieldSearchable
                 | ColLabFieldExp (HsonDocument -> LabelExp)

instance Show ColFieldExp where
  show ColFieldSearchable = "ColFieldSearchable"
  show (ColLabFieldExp _) = "ColLabFieldExp {- function -}"

-- | Labeled field expression solely contains a list of components.
type ColLabFieldExpS = Map String Component

-- | Labeled field expression composition monad.
newtype ColLabFieldExpM a =
  ColLabFieldExpM (ErrorT String (StateT ColLabFieldExpS (Reader (FieldName, CollectionName))) a)
  deriving (Monad, MonadState ColLabFieldExpS, MonadReader (FieldName, CollectionName))

instance Role Readers ColLabFieldExpS ColLabFieldExpM where 
  _ ==> c = ColLabFieldExpM $ do
    s <- get 
    (fName, cName) <- ask
    case Map.lookup (show readers) s of
      Just _ -> fail $ "Collection " ++ show cName ++ " field " ++ show fName
                          ++ " readers already specified."
      Nothing -> lift . put $ Map.insert (show readers) (toComponent c) s

instance Role Writers ColLabFieldExpS ColLabFieldExpM where 
  _ ==> c = ColLabFieldExpM $ do
    s <- get 
    (fName, cName) <- ask
    case Map.lookup (show writers) s of
      Just _ -> fail $ "Collection " ++ show cName ++ " field " ++ show fName
                          ++ " writers already specified."
      Nothing -> put $ Map.insert (show writers) (toComponent c) s

-- | Field expression composition monad.
newtype ColFieldExpM a =
  ColFieldExpM (ErrorT String (StateT (Maybe ColFieldExp) (Reader (FieldName, CollectionName))) a)
  deriving (Monad, MonadState (Maybe ColFieldExp), MonadReader (FieldName, CollectionName))

-- | Set the underlying field to be a searchable key.
--
-- >   field "name" searchable
searchable :: ColFieldExpM ()
searchable = do
  s <- get 
  (fName, cName) <- ask
  when (isJust s) $ fail $ "Collection " ++ show cName ++ " field " ++
                           show fName ++ " policy already specified."
  put (Just ColFieldSearchable)

-- | Synonym for 'searchable'
key :: ColFieldExpM ()
key = searchable

-- | Set data-dependent document label
--
-- >   field "password" $ labeled $ \doc -> do
-- >     readers ==> (("name" `at`doc) :: String)
-- >     writers ==> (("name" `at`doc) :: String)
labeled :: (HsonDocument -> ColLabFieldExpM ()) -> ColFieldExpM ()
labeled fpol = do
  s  <- get
  (fN, cN) <- ask
  when (isJust s) $ fail $ "Collection " ++ show cN ++ " field " ++
                           show fN ++ " policy already specified."
  let labFieldE = ColLabFieldExp $ \doc ->
                      fromRight $ eval (fpol' doc fN cN) fN cN
  put (Just labFieldE)
  where eval (ColLabFieldExpM e) fN cN =
          runReader (evalStateT (runErrorT e) Map.empty) (fN, cN)
        fpol' doc fN cN = do fpol doc
                             s <- get
                             r <- lookup' fN cN (show readers) s
                             w <- lookup' fN cN (show writers) s
                             return $ LabelExp r w
        lookup' fN cN k s = maybe (fail $ "Missing " ++ show k ++
                                          " in field label " ++ show fN
                                          ++ " of collection " ++ show cN)
                               return $ Map.lookup k s


--------------------------------------------------------------

-- | Type representing a collection expression.
--
-- > collection "w00t" $ do
-- >   access $ do
-- >     readers ==> "Alice" \/ "Bob"
-- >     writers ==> "Alice"          
-- >   clearance $ do
-- >     secrecy   ==> "Users"
-- >     integrity ==> "Alice"          
-- >   document $ \doc ->  do
-- >     readers ==> anybody
-- >     writers ==> "Alice" \/ (("name" `at`doc) :: String)
-- >   field "name" searchable
-- >   field "password" $ labeled $ \doc -> do
-- >     readers ==> (("name" `at`doc) :: String)
-- >     writers ==> (("name" `at`doc) :: String)
--
data ColExp = ColExp CollectionName ColAccExp
                                    ColClrExp
                                    ColDocExp
                                    (Map FieldName ColFieldExp)
  deriving Show

-- | Internal state of collection
data ColExpT = ColAccT ColAccExp
             | ColClrT ColClrExp
             | ColDocT ColDocExp
             | ColFldT ColFieldExp
             deriving Show


-- | Collection expression may contain an access label expression,
-- a collection label expression, etc.
type ColExpS = Map String ColExpT

-- | Database expression composition monad
newtype ColExpM a =
  ColExpM (ErrorT String (StateT ColExpS (Reader CollectionName)) a)
  deriving (Monad, MonadState ColExpS, MonadReader CollectionName)


--------------------------------------------------------------

-- | Type representing a policy
data PolicyExp = PolicyExp DBExp (Map CollectionName ColExp)
  deriving Show

-- | Internal state of policy
data PolicyExpT = PolicyDBExpT  DBExp
                | PolicyColExpT ColExp
                deriving Show

-- | Policy expression may contain a databse expression, or
-- a number of collection expressions.
type PolicyExpS = Map String PolicyExpT

-- | Policy expression composition monad
newtype PolicyExpM a = PolicyExpM (ErrorT String (State PolicyExpS) a)
  deriving (Monad, MonadState PolicyExpS)


--------------------------------------------------------------

-- | Set the collection access label. For example,
--
-- > collection "w00t" $ do
-- >   ...
-- >   access $ do
-- >     readers ==> "Alice" \/ "Bob"
-- >     writers ==> "Alice"
-- 
-- states that Alice and Bob can read documents from the collection,
-- but only Alice can insert new documents or modify existing ones.
access :: ColAccExpM () -> ColExpM ()
access (ColAccExpM acc) = do
  s  <- get
  cN <- ask
  case Map.lookup "access" s of
    Just _ -> fail $ "Collection " ++ show cN
                        ++ " access label already specified."
    _ -> let r = runReader (evalStateT (runErrorT (acc' cN)) Map.empty) cN
         in case r of
              Left e -> fail e
              Right accT -> put (Map.insert "access" accT s)
  where acc' cN= do
          acc 
          s <- get
          r <- lookup' cN (show readers) s
          w <- lookup' cN (show writers) s
          return . ColAccT $ ColAccExp r w
        lookup' cN k s = maybe (fail $ "Missing " ++ show k ++
                                       " in access of " ++ show cN)
                            return $ Map.lookup k s

-- | Set the collection clearance. For example,
--
-- > collection "w00t" $ do
-- >   ...
-- >   clearance $ do
-- >     secrecy ==> "Alice" \/ "Bob"
-- >     integrity ==> "Alice"
-- 
-- states that all data in the collection is always readable by Alice
-- and Bob, and no more trustworthy than data Alice can create.
clearance :: ColClrExpM () -> ColExpM ()
clearance (ColClrExpM acc) = do
  s  <- get
  cN <- ask
  case Map.lookup "clearance" s of
    Just _ -> fail $ "Collection " ++ show cN
                        ++ " clearance label already specified."
    _ -> let r = runReader (evalStateT (runErrorT (acc' cN)) Map.empty) cN
         in case r of
              Left e -> fail e
              Right accT -> put (Map.insert "clearance" accT s)
  where acc' cN = do
          acc 
          s <- get
          r <- lookup' cN (show readers) s
          w <- lookup' cN (show writers) s
          return . ColClrT $ ColClrExp r w
        lookup' cN k s = maybe (fail $ "Missing " ++ show k ++
                                       " in clearance of " ++ show cN)
                            return $ Map.lookup k s

-- | Set data-dependent document label. For example,
--
-- > collection "w00t" $ do
-- >   ...
-- >   document $ \doc ->  do
-- >     readers ==> anybody
-- >     writers ==> "Alice" \/ (("name" `at`doc) :: String)
--
-- states that every document in the collection is readable by anybody,
-- and only Alice or the principal named by the @name@ value in the
-- document can modify or insert such data.
document :: (HsonDocument -> ColDocExpM ()) -> ColExpM ()
document fpol = do
  s  <- get
  cN <- ask
  case Map.lookup "document" s of
    Just _ -> fail $ "Collection " ++ show cN
                        ++ " document policy already specified."
    _ -> let docT = ColDocT $ ColDocExp $ \doc ->
                                fromRight $ eval (fpol' doc cN) cN
         in put (Map.insert "document" docT s)
  where eval (ColDocExpM e) cN =
          runReader (evalStateT (runErrorT e) Map.empty) cN
        fpol' doc cN = do fpol doc
                          s <- get
                          r <- lookup' cN (show readers) s
                          w <- lookup' cN (show writers) s
                          return $ LabelExp r w
        lookup' cN k s = maybe (fail $ "Missing " ++ show k ++
                                       " in document label of collection " 
                                       ++ show cN)
                            return $ Map.lookup k s

-- | Set field policy. A field can be declared to be a 'searchable'
-- key or a 'labeled' value.
--
-- Declaring a field to be a 'searchable' key is straight forward:
--
-- > collection "w00t" $ do
-- >   ...
-- >   field "name" searchable
--
-- The 'labeled' field declaration is similar to the 'document' policy, but
-- sets the label of a specific field. For example
--
-- > collection "w00t" $ do
-- >   ...
-- >   field "password" $ labeled $ \doc -> do
-- >     let user = "name" `at` doc :: String
-- >     readers ==> user
-- >     writers ==> user
--
-- states that every @password@ field in the is readable and writable
-- only by or the principal named by the @name@ value of the document can
-- modify or insert such data.
field :: FieldName -> ColFieldExpM () -> ColExpM ()
field fName (ColFieldExpM e) = do
  s <- get
  cN <- ask
  let _fName = "field." ++ T.unpack fName
  case Map.lookup _fName s of
    Just _ -> fail $ "Collection " ++ show cN ++ " field " ++ show fName
                        ++ " policy already specified."
    _ -> case runReader (evalStateT (runErrorT e') Nothing) (fName, cN) of
           Left er -> fail er
           Right Nothing -> fail $ "Collection " ++ show cN ++ " field " ++
                              show fName ++ " policy not specified."
           Right (Just fieldE) -> put (Map.insert _fName (ColFldT fieldE) s)
      where e' = do e >> get
    

-- | Set the collection labels and policies. Each @collection@, must 
-- at least specify who can 'access' the collection, what the
-- 'clearance' of the data in the collection is, and how 'document's
-- are labeled. Below is an example that also labels the @password@
-- field and declares @name@ a searchable key.
--
-- > collection "w00t" $ do
-- >   access $ do
-- >     readers ==> "Alice" \/ "Bob"
-- >     writers ==> "Alice"          
-- >   clearance $ do
-- >     secrecy   ==> "Users"
-- >     integrity ==> "Alice"          
-- >   document $ \doc ->  do
-- >     readers ==> anybody
-- >     writers ==> "Alice" \/ (("name" `at`doc) :: String)
-- >   field "name" searchable
-- >   field "password" $ labeled $ \doc -> do
-- >     readers ==> (("name" `at`doc) :: String)
-- >     writers ==> (("name" `at`doc) :: String)
--
collection :: CollectionName -> ColExpM () -> PolicyExpM ()
collection cN (ColExpM e) = do
  s <- get
  let _cN = "collection." ++ T.unpack cN
  case Map.lookup _cN s of
    Just _ -> fail $ "Collection " ++ show cN ++ " policy already set"
    Nothing -> case runReader (evalStateT (runErrorT e') Map.empty) cN of
                 Left err -> fail err
                 Right colExp -> put $ Map.insert _cN (PolicyColExpT colExp) s
  where e' = do
          e
          s <- get
          (ColAccT a) <- lookup' "access" s
          (ColClrT c) <- lookup' "clearance" s
          (ColDocT d) <- lookup' "document" s
          let fs = Map.mapKeys (T.pack . (drop (length "field."))) $
                   Map.map (\(ColFldT f) -> f) $
                   Map.filterWithKey (\k _ -> "field." `isPrefixOf` k) s
          return $ ColExp cN a c d fs
        lookup' k s = maybe (fail $ "Missing " ++ show k ++
                                    " for collection " ++ show cN)
                         return $ Map.lookup k s


--------------------------------------------------------------

-- | Compile a policy.
runPolicy :: PolicyExpM () -> Either String PolicyExp
runPolicy (PolicyExpM e) = evalState (runErrorT e') Map.empty
  where e' = do
         e
         s <- get
         (PolicyDBExpT db) <- maybe (fail $ "Missing database policy")
                                    return $ Map.lookup "database" s
         let cs = Map.mapKeys (T.pack . (drop (length "collection."))) $
                  Map.map (\(PolicyColExpT f) -> f) $
                  Map.filterWithKey (\k _ -> "collection." `isPrefixOf` k) s
         return $ PolicyExp db cs

-- | High level function used to set the policy in a 'PolicyModule'.
-- This function takes the policy module's privileges and a policy
-- expression, and produces a 'PMAction' that sets the policy.
setPolicy :: DCPriv -> PolicyExpM () -> PMAction ()
setPolicy priv pol = 
  case runPolicy pol of
    Left err -> liftLIO $ throwLIO $ PolicyCompileError err
    Right policy -> execPolicy policy
  where execPolicy (PolicyExp db cs) = do
          execPolicyDB db 
          void $ forM cs execPolicyCol
        --
        execPolicyDB (DBExp r w a) = do
          setDatabaseLabelP priv (dcLabel r w)
          setCollectionSetLabelP priv (dcLabel r a)
        --
        execPolicyCol (ColExp n (ColAccExp lr lw) (ColClrExp cr cw) doc fs) =
          let cps = mkColPol doc fs
          in createCollectionP priv n (dcLabel lr lw) (dcLabel cr cw) cps
        --
        mkColPol (ColDocExp fdocE) cs = 
          let fdoc = unDataPolicy fdocE
          in CollectionPolicy { documentLabelPolicy = fdoc
                              , fieldLabelPolicies = Map.map unFieldExp cs }
        --
        unDataPolicy fpolE = \doc -> 
          let (LabelExp s i) = fpolE doc
          in dcLabel s i
        --
        unFieldExp ColFieldSearchable = SearchableField
        unFieldExp (ColLabFieldExp f) = FieldPolicy (unDataPolicy f)

-- | Exception thrown if a policy cannot be \"compiled\" or if we
-- deternmine that it's faulty at \"runtime\".
data PolicySpecificiationError = PolicyCompileError String
                               | PolicyRuntimeError String
  deriving (Show, Typeable)

instance Exception PolicySpecificiationError


--
-- Helpers

fromRight :: Either String b -> b
fromRight (Right x) = x
fromRight (Left e)  = throw . PolicyRuntimeError $ e
