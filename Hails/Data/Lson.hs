{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Hails.Data.Lson where

import Prelude hiding (lookup)
import Control.Monad.Identity
import Data.List (find, findIndex)
import Data.Maybe (mapMaybe)
import Data.Typeable hiding (cast)
import qualified Database.MongoDB as M
import LIO.Base

type Key = M.Label
type Value l = Labeled l M.Value
data Field l = (:=) {key :: Key, value :: Value l }
type Document l = Labeled l [Field l]

look :: (Monad m, Label l) => Key -> [Field l] -> m (Value l)
-- ^ Value of field in document, or fail (Nothing) if field not found
look k doc = maybe notFound (return . value) (find ((k ==) . key) doc) where
	notFound = fail $ "expected " ++ show k

lookup :: (M.Val v, Monad m, Label l) => Key -> [Field l] -> LIO l s (m v)
-- ^ Lookup value of field in document and cast to expected type. Fail (Nothing) if field not found or value not of expected type.
lookup k doc = do
  mval <- fmap unlabel $ look k doc
  fmap M.cast mval

valueAt :: Label l => Key -> [Field l] -> Value l
-- ^ Value of field in document. Error if missing.
valueAt k = runIdentity . look k

at :: forall s v l. (M.Val v, Label l) => Key -> [Field l] -> LIO l s v
-- ^ Typed value of field in document. Error if missing or wrong type.
at k doc = fmap (maybe err id) (lookup k doc)  where
	err = error $ "expected (" ++ show k ++ " :: " ++ show (typeOf (undefined :: v)) ++ ")"

include :: Label l => [Key] -> [Field l] -> [Field l]
-- ^ Only include fields of document in key list
include keys doc = mapMaybe (\k -> find ((k ==) . key) doc) keys

exclude :: Label l => [Key] -> [Field l] -> [Field l]
-- ^ Exclude fields from document in key list
exclude keys doc = filter (\(k := _) -> notElem k keys) doc

merge :: Label l => [Field l] -> [Field l] -> [Field l]
-- ^ Merge documents with preference given to first one when both have the same key. I.e. for every (k := v) in first argument, if k exists in second argument then replace its value with v, otherwise add (k := v) to second argument.
merge es doc = foldl f doc es where
	f doc (k := v) = case findIndex ((k ==) . key) doc of
		Nothing -> doc ++ [k := v]
		Just i -> let (x, _ : y) = splitAt i doc in x ++ [k := v] ++ y
