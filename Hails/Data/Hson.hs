{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             FlexibleContexts,
             DeriveDataTypeable,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             OverlappingInstances,
             FunctionalDependencies
             #-}

{- |

This module exports the type for a Hails BSON document, 'HsonDoc' and
related classes for creating such documents.  A Hails document is
similar to "Data.Bson"\'s documents, but differs in two ways. First,
Hails restricts the number of types to a subset of BSON's (see
'BsonVal'). This restriction is primarily due to the fact that many of
the BSON types are redundant and not used (at least within Hails).
Second, Hails allows for documents to contain policy-labeled values.

Policy labeled values ('PolicyLabeled') are permitted only at the
\"top-level\" of a document. (This is primarily done to keep
policy-specification simple and may change in the future.)
Consequently to allow for nested documents and documents containing an
array of values we separate top-level fields ('HsonField'), that may
contain policy labeled values, from potentially-nested fields
('BsonField'). A top-level field 'HsonField' is thus either a
'BsonField' or a 'PolicyLabled' value.

Example:

> module Main (x, y) where
> 
> import Data.Text (Text)
> 
> import LIO.DCLabel
> import LIO.Labeled.TCB (labelTCB)
> import Hails.Data.Hson
> 
> -- | Create document, verbose approach
> x :: HsonDocument
> x = [ "myInt"  =: BsonInt32 42
>     , "nested" =: BsonDoc [ "flag" =: BsonBool True]
>     , "secret" =: (HsonLabeled $ hasPolicy (labelTCB dcPub (BsonString "hi")))
>     ]
> 
> -- | Create same document, clean approach
> y :: HsonDocument
> y = [ "myInt" -: (42 :: Int)
>     , "nested"  -: ([ "flag" -: True] :: BsonDocument)
>     , "secret" -: labelTCB dcPub (toBsonValue ("hi" :: Text))
>     ]

Both @x@ and @y@ with @-XOverloadedStrings@:

> [myInt -: 42,nested -: [flag -: True],secret -: HsonLabeled]


-}

module Hails.Data.Hson (
  -- * Documents
    HsonDocument, Document, BsonDocument
  -- ** Operations on documents
  , DocOps(..)
  , DocValOps(..)
  , include, exclude
  , merge
  -- ** Converting to/from Hson/Bson
  , bsonDocToHsonDoc, bsonFieldToHsonField
  , isBsonDoc
  , hsonDocToBsonDoc
  , hsonDocToBsonDocStrict
  -- ** Converting labeled requests
  , labeledRequestToHson
  -- * Fields
  , FieldName, HsonField(..), BsonField(..)
  , IsField(..), Field(..), GenField(..)
  -- * Values
  , HsonValue(..), HsonVal(..)
  , BsonValue(..), BsonVal(..)
  , PolicyLabeled, needPolicy, hasPolicy, getPolicyLabeled 
  , Binary(..)
  , ObjectId(..), genObjectId
  ) where

import           Prelude hiding (lookup)
import           Control.Monad (liftM)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Binary.Put
import           Data.Bson.Binary
import           Data.Maybe (mapMaybe, fromJust, fromMaybe)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int32, Int64)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable
import           Data.Functor.Identity (runIdentity)
import qualified Data.Bson as Bson

import           Data.Conduit (runResourceT, ($$))
import           Data.Conduit.Binary (sourceLbs)

import           LIO
import           LIO.DCLabel
import           LIO.TCB

import           Network.Wai.Parse ( FileInfo(..)
                                   , sinkRequestBody
                                   , lbsBackEnd)

import           Hails.Data.Hson.TCB
import           Hails.HttpServer.Types

infix 0 =:, -:


-- | Synonym for 'HsonDocument'
type Document = HsonDocument


--
-- Fields
--

instance Show BsonField where
  show (BsonField n v) = T.unpack n ++ " -: " ++ show v

instance Show HsonField where
  show (HsonField n v) = T.unpack n ++ " -: " ++ show v

-- | Class for retrieving the name of a field.
class IsField f where
  -- | Get the name of a field.
  fieldName :: f -> FieldName

instance IsField BsonField where fieldName (BsonField n _) = n
instance IsField HsonField where fieldName (HsonField n _) = n

-- | Class used to define fields.
class (IsField f, Show v, Show f) => Field v f where
  -- | Given a name and value construct either a 'HsonField' or
  -- 'BsonField'
  (=:) :: FieldName -> v -> f
  -- | Get the field value.
  fieldValue :: Monad m => f -> m v

instance Field BsonValue BsonField where
  (=:) = BsonField
  fieldValue (BsonField _ v) = return v
instance Field BsonValue HsonField where
  n =: v = n =: HsonValue v
  fieldValue (HsonField _ (HsonValue v)) = return v
  fieldValue _ = fail "fieldValue: cannot extract PolicyLabled"
instance Field HsonValue HsonField where
  (=:) = HsonField
  fieldValue (HsonField _ v) = return v


--
-- Values
--


instance Show BsonValue where
  show (BsonFloat v)  = show v
  show (BsonString v) = show v
  show (BsonDoc v)    = show v
  show (BsonArray v)  = show v
  show (BsonBlob v)   = show v
  show (BsonObjId v)  = show v
  show (BsonBool v)   = show v
  show (BsonUTC v)    = show v
  show BsonNull       = "NULL"
  show (BsonInt32 v)  = show v
  show (BsonInt64 v)  = show v

instance Show HsonValue where
  show (HsonValue   h)  = show h
  show (HsonLabeled _) = "{- Hidden -} HsonLabeled"

instance ShowTCB HsonValue where
  showTCB (HsonValue   h) = show h
  showTCB (HsonLabeled h) = showTCB h



--
-- Policy labeled values
--

instance ShowTCB PolicyLabeled where
  showTCB (NeedPolicyTCB bv) = "NeedPolicyTCB " ++ show bv
  showTCB (HasPolicyTCB lbv) = "HasPolicyTCB " ++ showTCB lbv

-- | Create a policy labeled value given an unlabeled 'HsonValue'.
needPolicy :: BsonValue-> PolicyLabeled
needPolicy = NeedPolicyTCB

-- | Create a policy labeled value a labeled 'HsonValue'.
hasPolicy :: DCLabeled BsonValue -> PolicyLabeled
hasPolicy = HasPolicyTCB

-- | Get the policy labeled value, only if the policy has been
-- applied.
getPolicyLabeled :: Monad m => PolicyLabeled -> m (DCLabeled BsonValue)
getPolicyLabeled (NeedPolicyTCB _) =
  fail "Can only retrieve already labeldv policy values"
getPolicyLabeled (HasPolicyTCB lv) = return lv

--
-- Document operations
--

-- | Class used to implement operatoins on documents that return
-- 'HsonValue's or 'BsonValue's. The main role of this function is to
-- impose the functional dependency between values and fields.  As a
-- consequence 'look'ing up and getting 'valueAt' in a 'HsonDocument'
-- (resp. 'BsonDocument') will return a 'HsonValue' (resp. 'BsonValue').
-- This eliminates the need to specify the end type of very query, but
-- forces the programmer to cast between Hson and Bson values.
class Field v f => DocOps v f | v -> f, f -> v where
  -- | Find value of field in document, or fail not found.
  look :: (Field v f, Monad m) => FieldName -> [f] -> m v
  look n doc = case List.find ((==n) . fieldName) doc of
                 Just v -> fieldValue v
                 _      -> fail $ "look: Not found " ++ show n

  -- | Same as 'look', but 'fail's if the value is not found.
  valueAt :: Field v f => FieldName -> [f] -> v
  valueAt n = runIdentity . look n

  -- Serializes a document to binary BSON representation
  serialize :: [f] -> L8.ByteString

instance DocOps HsonValue HsonField where
  serialize = serialize . hsonDocToBsonDoc 

instance DocOps BsonValue BsonField where
  serialize = runPut . putDocument . bsonDocToDataBsonDocTCB

-- | Only include fields specified.
include :: IsField f => [FieldName] -> [f] -> [f]
include ns doc = mapMaybe (\n -> List.find ((==n) . fieldName) doc) ns

-- | Exclude fields specified.
exclude :: IsField f => [FieldName] -> [f] -> [f]
exclude ns doc = filter ((\n -> notElem n ns) . fieldName) doc

-- | Merge documents with preference given to first one when both have
-- the same field name.
merge :: IsField f => [f] -> [f] -> [f]
merge doc1 doc2 = 
  let ns1 = map fieldName doc1
      doc2' = List.filter ((\n -> n `notElem` ns1) . fieldName) doc2
  in doc1 ++ doc2'

-- | Class used to implement operations on documents that return
-- Haskell values (as opposed to 'HsonValue' or 'BsonValue').
class DocValOps d v where
  -- | Same as 'look', but returns \"unwrapped\" value.
  lookup :: (Monad m) => FieldName -> d -> m v
  -- | Same as 'valueAt', but returns \"unwrapped\" value.
  at :: FieldName -> d -> v
  at n = runIdentity . lookup n

instance HsonVal v => DocValOps HsonDocument v where
  lookup n doc = look n doc >>= fromHsonValue

instance BsonVal v => DocValOps BsonDocument v where
  lookup n doc = look n doc >>= fromBsonValue


--
-- Converters
--

-- | Returns true if the document is composed solely of 'BsonValue's.
-- This function is useful when converting from 'HsonDocument' to
-- 'BsonDocument'.
isBsonDoc :: HsonDocument -> Bool
isBsonDoc doc = all f doc
  where f (HsonField _ (HsonLabeled _)) = False
        f _ = True

-- | Convert 'BsonDocument' to 'HsonDocument'
bsonDocToHsonDoc :: BsonDocument -> HsonDocument
bsonDocToHsonDoc = map bsonFieldToHsonField 

-- | Convert 'BsonField' to 'HsonField'
bsonFieldToHsonField :: BsonField -> HsonField
bsonFieldToHsonField (BsonField n v) = n =: v

-- | This is a relaxed version of 'hsonDocToBsonDocStrict' that only
-- converts fields containing 'BsonValue's. In other words, the
-- 'PolicyLabeled' values are dropped.
hsonDocToBsonDoc :: HsonDocument -> BsonDocument
hsonDocToBsonDoc doc = mapMaybe hsonFieldToBsonField doc

-- | Convert an 'HsonDocument' to a 'BsonDocument'. If any of the
-- fields contain 'PolicyLabeled' values (i.e., are 'HsonLabeled'
-- values) this function 'fail's, otherwise it returns the converted
-- document. To check for failure use 'isBsonDoc'.
hsonDocToBsonDocStrict :: Monad m => HsonDocument -> m BsonDocument
hsonDocToBsonDocStrict doc = mapM hsonFieldToBsonField doc

-- | Convert 'HsonField' to 'BsonField'
hsonFieldToBsonField :: Monad m => HsonField -> m BsonField
hsonFieldToBsonField (HsonField n (HsonValue v)) = return (n =: v)
hsonFieldToBsonField (HsonField n _) = 
  fail $ "hsonFieldToBsonField: field " ++ show n ++ " is PolicyLabeled"


-- | Convert a labeled request to a labeled document. Values of fields that
-- have a name that ends with @[]@ are converted to arrays and the
-- suffix @[]@ is stripped from the name.
labeledRequestToHson :: MonadLIO DCLabel m
                     => DCLabeled Request -> m (DCLabeled HsonDocument)
labeledRequestToHson lreq = liftLIO $ do
  let (LabeledTCB origLabel req) = lreq
      btype     = fromMaybe UrlEncoded $ getRequestBodyType req
  (ps, fs) <- liftLIO . ioTCB $ runResourceT $
                sourceLbs (requestBody req) $$ sinkRequestBody lbsBackEnd btype
  let psDoc     = map convertPS ps
      fsDoc     = map convertFS fs
  return $ LabeledTCB origLabel $ arrayify $ psDoc ++ fsDoc
  where convertPS (k,v) = HsonField
                           (T.pack . S8.unpack $ k)
                           (toHsonValue . S8.unpack $ v)
        convertFS (k,v) = HsonField (T.pack . S8.unpack $ k) 
                                    (toHsonValue $ fsToDoc v)
        fsToDoc f = [ "fileName" -: (T.pack . S8.unpack $ fileName f)
                    , "fileContentType" -:
                      (T.pack . S8.unpack $ fileContentType f)
                    , "fileContent" -: (S8.concat . L.toChunks $ fileContent f)
                    ] :: BsonDocument
        arrayify doc =
          let pred0 = (T.isSuffixOf "[]" . fieldName)
              (doc0, doc0_keep) = List.partition pred0 doc
              gs = List.groupBy (\x y -> fieldName x == fieldName y)
                 . List.sortBy (\x y -> fieldName x `compare` fieldName y)
                 $ doc0
              doc1 = concat $ map toArray gs
          in doc0_keep ++ doc1
            where toArray [] = []
                  toArray ds = let n = fromJust
                                     . T.stripSuffix "[]"
                                     . fieldName
                                     . head $ ds
                                   vs = map (fromJust .  fieldValue) ds
                               in [ n -: BsonArray vs ]

--
-- Friendly interface for creating documents
--

-- | Class used to define fields.
class (Show v, Show f) => GenField v f where
  -- | Given a name and Haskell value construct either a 'HsonField'
  -- or a 'BsonField'
  (-:) :: FieldName -> v -> f

instance BsonVal v => GenField v BsonField where
  n -: v = n =: toBsonValue v
instance HsonVal v => GenField v HsonField where
  n -: v = n =: toHsonValue v


--
-- To/From BsonValue
--

-- | Class used to (de)construct 'BsonValue's
class (Typeable a, Show a) => BsonVal a where
  -- | Convert to 'BsonValue'
  toBsonValue   :: a -> BsonValue
  -- | Convert from 'BsonValue'
  fromBsonValue :: Monad m => BsonValue -> m a

instance BsonVal Double where
  toBsonValue = BsonFloat
  fromBsonValue (BsonFloat x) = return x
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Double of " ++ (show v)

instance BsonVal Float where
  toBsonValue = BsonFloat . realToFrac
  fromBsonValue (BsonFloat x) = return (realToFrac x)
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Float of " ++ (show v)

instance BsonVal Text where
  toBsonValue = BsonString
  fromBsonValue (BsonString x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Text of " ++ (show v)

instance BsonVal String where
  toBsonValue = BsonString . T.pack
  fromBsonValue (BsonString x) = return $ T.unpack x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to String of " ++ (show v)

instance BsonVal S8.ByteString where
  toBsonValue = BsonString . T.pack . S8.unpack
  fromBsonValue (BsonString x) = return $ S8.pack $ T.unpack x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to ByteString (Strict) of " ++ (show v)

instance BsonVal L8.ByteString where
  toBsonValue = BsonString . T.pack . L8.unpack
  fromBsonValue (BsonString x) = return $ L8.pack $ T.unpack x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to ByteString (Lazy) of " ++ (show v)

instance BsonVal BsonDocument where
  toBsonValue = BsonDoc
  fromBsonValue (BsonDoc x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to BsonDocument of " ++ (show v)

instance BsonVal [BsonValue] where
  toBsonValue = BsonArray
  fromBsonValue (BsonArray x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to [BsonValue] of " ++ (show v)

instance (BsonVal a) => BsonVal [a] where
  toBsonValue = BsonArray . map toBsonValue
  fromBsonValue (BsonArray x) = mapM fromBsonValue x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to [a] of " ++ (show v)

instance BsonVal Binary where
  toBsonValue = BsonBlob
  fromBsonValue (BsonBlob x) = return x
  fromBsonValue (BsonString x) = return $ Binary $ S8.pack $ T.unpack x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Binary of " ++ (show v)


instance BsonVal ObjectId where
  toBsonValue = BsonObjId
  fromBsonValue (BsonObjId x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to ObjectId of " ++ (show v)

instance BsonVal Bool where
  toBsonValue = BsonBool
  fromBsonValue (BsonBool x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Bool of " ++ (show v)

instance BsonVal UTCTime where
  toBsonValue = BsonUTC
  fromBsonValue (BsonUTC x) = return x
  fromBsonValue v = fail $ "fromBsonValue: no conversion to UTCTime of " ++ (show v)

instance BsonVal (Maybe BsonValue) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just v) = v
  fromBsonValue BsonNull = return Nothing
  fromBsonValue v        = return (Just v)

instance (BsonVal a) => BsonVal (Maybe a) where
  toBsonValue Nothing  = BsonNull
  toBsonValue (Just a) = toBsonValue a
  fromBsonValue BsonNull = return Nothing
  fromBsonValue v = Just `liftM` fromBsonValue v

instance BsonVal Int32 where
  toBsonValue = BsonInt32
  fromBsonValue (BsonInt32 x) = return x
  fromBsonValue (BsonInt64 x) = fitInt x
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Int32 of " ++ (show v)

instance BsonVal Int64 where
  toBsonValue = BsonInt64
  fromBsonValue (BsonInt64 x) = return x
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Int64 of " ++ (show v)

instance BsonVal Int where
  toBsonValue n = maybe (BsonInt64 $ fromIntegral n) BsonInt32 (fitInt n)
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromEnum x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Int of " ++ (show v)

instance BsonVal Integer where
  toBsonValue n = maybe (maybe err BsonInt64 $ fitInt n)
                        BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for BsonInt{32,64}"
  fromBsonValue (BsonInt32 x) = return (fromIntegral x)
  fromBsonValue (BsonInt64 x) = return (fromIntegral x)
  fromBsonValue (BsonFloat x) = return (round x)
  fromBsonValue v = fail $ "fromBsonValue: no conversion to Integer of " ++ (show v)

--
-- To/From HsonValue
--
-- Lot of redundant instances to avoid use of gnarly GHC extensions
--

-- | Class used to (de)construct 'HsonValue's
class (Typeable a, Show a) => HsonVal a where
  -- | Convert to 'HsonValue'
  toHsonValue   :: a -> HsonValue
  -- | Convert from 'HsonValue'
  fromHsonValue :: Monad m => HsonValue -> m a

instance HsonVal HsonValue where
  toHsonValue = id
  fromHsonValue = return

instance HsonVal Double where
  toHsonValue = HsonValue . BsonFloat
  fromHsonValue (HsonValue (BsonFloat x)) = return x
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Float where
  toHsonValue = HsonValue . BsonFloat . realToFrac
  fromHsonValue (HsonValue (BsonFloat x)) = return (realToFrac x)
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Text where
  toHsonValue = HsonValue . BsonString
  fromHsonValue (HsonValue (BsonString x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal String where
  toHsonValue = HsonValue . BsonString . T.pack
  fromHsonValue (HsonValue (BsonString x)) = return $ T.unpack x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal BsonDocument where
  toHsonValue = HsonValue . BsonDoc
  fromHsonValue (HsonValue (BsonDoc x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal [BsonValue] where
  toHsonValue = HsonValue . BsonArray
  fromHsonValue (HsonValue (BsonArray x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance (HsonVal a, BsonVal a) => HsonVal [a] where
  toHsonValue = HsonValue . BsonArray . map toBsonValue
  fromHsonValue (HsonValue (BsonArray x)) = mapM fromBsonValue x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Binary where
  toHsonValue = HsonValue . BsonBlob
  fromHsonValue (HsonValue (BsonBlob x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal ObjectId where
  toHsonValue = HsonValue . BsonObjId
  fromHsonValue (HsonValue (BsonObjId x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Bool where
  toHsonValue = HsonValue . BsonBool
  fromHsonValue (HsonValue (BsonBool x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal UTCTime where
  toHsonValue = HsonValue . BsonUTC
  fromHsonValue (HsonValue (BsonUTC x)) = return x
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal (Maybe BsonValue) where
  toHsonValue Nothing = HsonValue BsonNull
  toHsonValue (Just v) = HsonValue v
  fromHsonValue (HsonValue BsonNull) = return Nothing
  fromHsonValue (HsonValue v)        = return (Just v)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance (HsonVal a, BsonVal a) => HsonVal (Maybe a) where
  toHsonValue Nothing  = HsonValue BsonNull
  toHsonValue (Just a) = HsonValue $ toBsonValue a
  fromHsonValue (HsonValue BsonNull) = return Nothing
  fromHsonValue (HsonValue v)        = fromBsonValue v
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int32 where
  toHsonValue = HsonValue . BsonInt32
  fromHsonValue (HsonValue (BsonInt32 x)) = return x
  fromHsonValue (HsonValue (BsonInt64 x)) = fitInt x
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int64 where
  toHsonValue = HsonValue . BsonInt64
  fromHsonValue (HsonValue (BsonInt64 x)) = return x
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Int where
  toHsonValue n = HsonValue $ maybe (BsonInt64 $ fromIntegral n)
                                    BsonInt32 (fitInt n)
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromEnum x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal Integer where
  toHsonValue n = HsonValue $ maybe (maybe err BsonInt64 $ fitInt n)
                                    BsonInt32 (fitInt n) 
    where err = error $ show n ++ " is too large for HsonInt{32,64}"
  fromHsonValue (HsonValue (BsonInt32 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonInt64 x)) = return (fromIntegral x)
  fromHsonValue (HsonValue (BsonFloat x)) = return (round x)
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal BsonValue where
  toHsonValue = HsonValue
  fromHsonValue (HsonValue v) = return v
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance HsonVal PolicyLabeled where
  toHsonValue   = HsonLabeled
  fromHsonValue (HsonLabeled v) = return v
  fromHsonValue _ = fail "fromHsonValue: no conversion"

instance Show (DCLabeled BsonValue) where
  show lv = " -UNKNOWN VALUE- {" ++ show (labelOf lv) ++ "}"
  
instance HsonVal (DCLabeled BsonValue) where
  toHsonValue   = HsonLabeled . hasPolicy
  fromHsonValue (HsonLabeled v) = getPolicyLabeled v
  fromHsonValue _ = fail "fromHsonValue: no conversion"


--
-- ObjectId
--

-- | Create a fresh ObjectId.
genObjectId :: MonadLIO DCLabel m => m ObjectId
genObjectId = liftLIO $ ioTCB Bson.genObjectId

--
-- Helpers
--
  

-- | From "Data.Bson": Cast number to end type, if it \"fits\".
fitInt :: forall a b m. (Integral a, Integral b, Bounded b, Monad m)
        => a -> m b
fitInt n = if fromIntegral (minBound :: b) <= n
              && n <= fromIntegral (maxBound :: b)
             then return $ fromIntegral n
             else fail "fitInt: number is too big"

