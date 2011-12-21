module Hails.Network.Http (
     simpleLHTTP,
     getRequest,
    ) where

import Network.HTTP
import DCLabel.NanoEDSL
import LIO.DCLabel
import LIO.TCB

simpleLHTTP :: HStream ty => Request ty -> DC (Maybe ty)
simpleLHTTP request = do
  current <- seq request getLabel
  if current `leq` (newDC (<>) (<>)) then
    ioTCB $ do
      resp <- simpleHTTP request
      fmap Just $ getResponseBody resp
    else
      return Nothing
