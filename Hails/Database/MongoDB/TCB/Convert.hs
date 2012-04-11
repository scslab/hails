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
-- input. Arguments values are parsed in to BSON Strings except if the
-- key is of the form \"key_name[]\" in which case all such arguments
-- will be combined into an array of Strings.
labeledDocI :: (LabelState l p s)
                  => HttpReq a
                  -> Labeled l L.ByteString
                  -> LIO l p s (Labeled l (Document l))
labeledDocI req lbody = do
  let lbl = labelOf lbody
  doc <- enumPure (unlabelTCB lbody) |$ formFolder req
  return $ labelTCB lbl doc

-- | Parases query or request body into a BSON document. Query components
-- become 'Key' 'String' pairs in the BSON doc. If a query argument has the
-- form \"key1[]=blah\" it will be parsed as an array of 'String's and equally
-- named arguments will be combined.
formFolder :: (LabelState l p s)
           => HttpReq a -> Iter L.ByteString (LIO l p s) (Document l)
formFolder req = foldForm req docontrol []
  where docontrol acc field = do
          formVal <- fmap L.unpack pureI
          let k = S.unpack $ ffName field
          if endswith "[]" k then
            return $ appendVal acc k formVal
            else do
              let lfld = pack (S.unpack.ffName $ field) =: formVal
              return $ lfld : acc

-- | Appends the a value to the corresponding field in a document. If the field
-- already exists in the document, appends the value to the array. Otherwise the
-- field is added with the passed in value the only element in the array.
appendVal :: LabelState l p s => Document l -> String -> String -> Document l
appendVal doc k' formVal =
  let k = U.pack $ takeWhile (/= '[') k'
      field = (k := BsonVal (B.Array [B.String $ U.pack formVal]))
  in case find (isKey k) doc of
        Just _ -> map (upsert k) doc
        Nothing -> field:doc
  where upsert k f@(k0 := (BsonVal (B.Array arr))) =
          if k0 == k then
            (k =: (B.String $ U.pack formVal):arr)
            else f
        upsert _ f = f
        isKey kk (k := _) = k == kk

