module Hails.Database.MongoDB.Unsafe where

import Control.Applicative
import Data.Either
import Data.UString (pack)
import qualified Database.MongoDB as M
import Hails.Data.Lson
import LIO.TCB
import LIO.DCLabel

type Cursor = M.Cursor
type Collection = M.Collection
data Database = Database M.Database

toMongoDatabase :: Database -> M.Database
toMongoDatabase (Database d) = d

class Policy p where
  database :: p -> Database
  colPolicy :: p -> M.Label -> M.Value -> DC (Field DCLabel)
  rowPolicy :: p -> [Field DCLabel] -> DC (Document DCLabel)

colPolicy' :: Policy p => p -> M.Field -> DC (Field DCLabel)
colPolicy' policy (l M.:= v) = colPolicy policy l v

applyPolicy :: Policy p => p -> M.Document -> DC (Document DCLabel)
applyPolicy policy row = do
  lfields <- mapM (colPolicy' policy) row
  rowPolicy policy lfields

newtype Action p a = Action { runAction :: p -> M.Action IO a }

instance Monad (Action p) where
  first >>= last = Action $ \policy -> do
    res <- (runAction first policy)
    runAction (last res) policy
  return foo = Action $ \_ -> return foo

nextWithPolicy :: Policy p => Cursor -> p -> M.Action IO (Maybe (DC (Document DCLabel)))
nextWithPolicy cursor policy = do
  row <- M.next cursor
  return $ fmap (applyPolicy policy) row

next :: Policy p => Cursor -> Action p (Maybe (DC (Document DCLabel)))
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

