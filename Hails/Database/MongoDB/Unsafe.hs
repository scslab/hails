module Hails.Database.MongoDB.Unsafe where

import Control.Applicative
import Data.Either
import Data.UString (pack)
import qualified Database.MongoDB as M
import Hails.Data.Lson
import LIO.TCB

type Cursor = M.Cursor
type Collection = M.Collection
data Database = Database M.Database

toMongoDatabase :: Database -> M.Database
toMongoDatabase (Database d) = d

data Policy l s = Policy {
    database :: Database
  , colPolicy :: M.Label -> M.Value -> LIO l s (Field l)
  , rowPolicy :: [Field l] -> LIO l s (Document l)
}

colPolicy' :: Label l => Policy l s -> M.Field -> LIO l s (Field l)
colPolicy' policy (l M.:= v) = colPolicy policy l v

applyPolicy :: Label l => Policy l s -> M.Document -> LIO l s (Document l)
applyPolicy policy row = do
  lfields <- mapM (colPolicy' policy) row
  rowPolicy policy lfields

newtype Action l s a = Action { runAction :: Policy l s -> M.Action IO a }

instance Monad (Action l s) where
  first >>= last = Action $ \policy -> do
    res <- (runAction first policy)
    runAction (last res) policy
  return foo = Action $ \_ -> return foo

nextWithPolicy :: Label l => Cursor -> Policy l s -> M.Action IO (Maybe (LIO l s (Document l)))
nextWithPolicy cursor policy = do
  row <- M.next cursor
  return $ fmap (applyPolicy policy) row

next :: Label l => Cursor -> Action l s (Maybe (LIO l s (Document l)))
next cursor = Action $ nextWithPolicy cursor

-- Utilities
server :: IO M.Pipe
server = M.runIOE $ M.connect (M.host "127.0.0.1")

run :: Label l => Policy l s -> Action l s a -> LIO l s (Either M.Failure a)
run policy action = rtioTCB $ do
  pipe <- server
  e <- M.access pipe M.master
        (toMongoDatabase $ database policy)
        (runAction action policy)
  M.close pipe
  return e

getCollection :: Label l => Collection -> Action l s Cursor
getCollection collection = Action $ \_ -> do
  cursor <- M.find $ M.select [] collection
  return $ cursor

