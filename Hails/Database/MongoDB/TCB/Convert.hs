{-# LANGUAGE Trustworthy #-}
module Hails.Database.MongoDB.TCB.Convert ( -- * Converting HTTP requests
                                            -- to 'Labeled' 'Document'
                                            labeledDocI
                                          ) where

import LIO
import LIO.TCB
import qualified Data.Bson as B (Value(..))
import qualified Data.UString as U
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.List.Utils
import Data.UString (pack)
import Hails.Data.LBson.TCB


-- | Trusted transformer that takes a 'Labeled' tuple with 'HttpReq'
-- and the request body as a 'L.ByteString' and returns a 'Labeled'
-- 'Document' with keys and values corresponding to the form fields
-- from the request. The label on the @Labeled@ result is the same as
-- input.
labeledDocI :: (LabelState l p s)
                  => HttpReq a
                  -> Labeled l L.ByteString
                  -> LIO l p s (Labeled l (Document l))
labeledDocI req lbody = do
  let lbl = labelOf lbody
  doc <- enumPure (unlabelTCB lbody) |$ formFolder req
  return $ labelTCB lbl doc

formFolder :: (LabelState l p s)
           => HttpReq a -> Iter L.ByteString (LIO l p s) (Document l)
formFolder req = foldForm req docontrol []
  where docontrol acc field = do
          formVal <- fmap L.unpack pureI
          let k = S.unpack $ ffName field
          case k of
            _ | endswith "[]" k -> return $ appendVal acc k formVal
              | otherwise -> do
              let lfld = pack (S.unpack.ffName $ field) =: formVal
              return $ lfld : acc

appendVal :: LabelState l p s => [Field l] -> String -> String -> [Field l]
appendVal doc k' formVal =
  let k = U.pack $ takeWhile (/= '[') k'
      field = (k := BsonVal (B.Array [B.String $ U.pack formVal]))
  in case find (isKey k) doc of
      Just (_ := (BsonVal (B.Array arr))) -> (k =: ((B.String $ U.pack formVal):arr)):(deleteBy (\x y -> (key x) == (key y)) field doc)
      _ -> field:doc
  where isKey kk (k := _) = k == kk
