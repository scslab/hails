module Hails.Network.Http (
     simpleLHTTP,
     module Network.HTTP,
    ) where

import Network.HTTP
import DCLabel.NanoEDSL
import LIO.DCLabel
import LIO.TCB

simpleLHTTP :: HStream ty => Request ty -> DC (Maybe ty)
simpleLHTTP request = do
  let regName = uriRegName $ reqURIAuth -- domain name without port
  let outputLabel = newDC (reg)
  current <- seq request getLabel
  if current `leq` outputLabel then
    ioTCB $ do
      resp <- simpleHTTP request
      fmap Just $ getResponseBody resp
    else
      return Nothing
