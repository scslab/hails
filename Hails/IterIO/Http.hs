{-# LANGUAGE Trustworthy #-}

module Hails.IterIO.Http (labeledBodyI) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO
import Data.IterIO.Http
import LIO.DCLabel
import LIO.TCB
import Hails.TCB.Types

type L = L8.ByteString

labeledBodyI :: HttpReq AppSessionData
      -> Iter L DC (DCLabeled (HttpReq AppSessionData, L))
labeledBodyI req = do
  bdy <- pureI
  let (AppSessionDataTCB user) = reqSession req
  return $ labelTCB (newDC (<>) (user)) (req, bdy)

