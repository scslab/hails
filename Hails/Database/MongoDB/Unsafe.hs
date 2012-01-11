module Hails.Database.MongoDB.Unsafe where

import Control.Applicative
import Data.Either
import Data.UString (pack)
import qualified Database.MongoDB as M
import LIO.TCB
import LIO.DCLabel

type Cursor = M.Cursor
type Value = Labeled DCLabel M.Value
data Field = (:=) M.Label Value
type Document = Labeled DCLabel [Field]
type Collection = M.Collection
data Database = Database M.Database

toMongoDatabase :: Database -> M.Database
toMongoDatabase (Database d) = d

class Policy p where
  database :: p -> Database
  colPolicy :: p -> M.Label -> M.Value -> DC Field
  rowPolicy :: p -> [Field] -> DC Document

applyColPolicy :: Policy p => p -> M.Document -> DC [Field]
applyColPolicy policy doc = go doc
  where go (field:fields) = do
          result <- labelField field
          rest <- go fields
          return (result:rest)
        go [] = return []
        labelField ((M.:=) l v) = colPolicy policy l v

newtype Action p a = Action { runAction :: p -> M.Action IO a }

instance Monad (Action p) where
  first >>= last = Action $ \policy -> do
    res <- (runAction first policy)
    runAction (last res) policy
  return foo = Action $ \_ -> return foo

nextWithPolicy :: Policy p => Cursor -> p -> M.Action IO (Maybe (DC Document))
nextWithPolicy cursor policy = do
  row <- M.next cursor
  return $ fmap applyPolicies row
  where applyPolicies row = do
          lfields <- applyColPolicy policy row
          rowPolicy policy lfields


next :: Policy p => Cursor -> Action p (Maybe (DC Document))
next cursor = Action $ nextWithPolicy cursor

-- Utilities
server :: IO M.Pipe
server = M.runIOE $ M.connect (M.host "127.0.0.1")

run :: Policy p => p -> Action p a -> DC (Either M.Failure a)
run policy action = rtioTCB $ do
  pipe <- server
  e <- M.access pipe M.master
        (toMongoDatabase $ database policy)
        (runAction action policy)
  M.close pipe
  return e

getCollection :: Policy p => Collection -> Action p Cursor
getCollection collection = Action $ \_ -> do
  cursor <- M.find $ M.select [] collection
  return $ cursor

