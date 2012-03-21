module Hails.Database.MongoDB.TCB.Convert ( -- * Converting HTTP requests
                                            -- to 'Labeled' 'Document'
                                            labeledDocI
                                          ) where

import LIO
import LIO.TCB
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.UString (pack)
import Hails.TCB.Types
import Hails.Data.LBson.TCB


-- | Trusted transformer that takes a 'Labeled' tuple with 'HttpReq'
-- and the request body as a 'L.ByteString' and returns a 'Labeled'
-- 'Document' with keys and values corresponding to the form fields
-- from the request. The label on the @Labeled@ result is the same as
-- input.
labeledDocI :: (LabelState l p s)
                  => HttpReq (AppSessionData l)
                  -> Iter L.ByteString (LIO l p s) (Labeled l (Document l))
labeledDocI req = do
  doc <- foldForm req docontrol []
  let (AppSessionDataTCB userL) = reqSession req
  return $ labelTCB userL doc
  where docontrol acc field = do
          formVal <- fmap L.unpack pureI
          let lfld = pack (S.unpack.ffName $ field) =: formVal
          return $ lfld : acc

