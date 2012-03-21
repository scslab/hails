module Hails.Database.MongoDB.TCB.Convert ( -- * Converting HTTP requests
                                            -- to 'Labeled' 'Document'
                                            toLabeledDocument
                                          ) where

import LIO
import LIO.TCB
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.UString (pack)
import Hails.Data.LBson.TCB


-- | Trusted transformer that takes a 'Labeled' tuple with 'HttpReq'
-- and the request body as a 'L.ByteString' and returns a 'Labeled'
-- 'Document' with keys and values corresponding to the form fields
-- from the request. The label on the @Labeled@ result is the same as
-- input.
toLabeledDocument :: (LabelState l p s)
                  => Labeled l (HttpReq b, L.ByteString)
                  -> LIO l p s (Labeled l (Document l))
toLabeledDocument lbld = do
  let (req, body) = unlabelTCB lbld
  doc <- enumPure body |$ foldForm req docontrol []
  return $ labelTCB (labelOf lbld) doc
  where docontrol acc field = do
          formVal <- fmap L.unpack pureI
          let lfld = pack (S.unpack.ffName $ field) =: formVal
          return $ lfld : acc

