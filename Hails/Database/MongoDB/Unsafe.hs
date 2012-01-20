module Hails.Database.MongoDB.Unsafe where

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
  , collection :: Collection
  , colPolicy :: M.Label -> M.Document -> l
  , rowPolicy :: [Field l] -> l
}

colPolicy' :: Label l => Policy l s -> M.Document -> M.Field -> LIO l s (Field l)
colPolicy' policy row field = do
  labelVal <- label (colPolicy policy (M.label field) row) (M.value field)
  return $ (M.label field) := labelVal

applyPolicy :: Label l => Policy l s -> M.Document -> LIO l s (Document l)
applyPolicy policy row = do
  lfields <- mapM (colPolicy' policy row) row
  let rowLabel = rowPolicy policy lfields
  label rowLabel lfields

newtype Action l s a = Action { runAction :: Policy l s -> M.Action IO a }

instance Monad (Action l s) where
  front >>= back = Action $ \policy -> do
    res <- (runAction front policy)
    runAction (back res) policy
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

getCollection :: Label l => Action l s Cursor
getCollection = Action $ \policy -> do
  cursor <- M.find $ M.select [] $ collection policy
  return $ cursor

