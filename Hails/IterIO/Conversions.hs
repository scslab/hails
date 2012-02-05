module Hails.IterIO.Conversions (iterIOtoIterLIO, onumIOtoOnumLIO) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import LIO.TCB

type L = L.ByteString

iterIOtoIterLIO :: (LabelState l p s, ChunkData a) => Iter a IO b -> Iter a (LIO l p s) b
iterIOtoIterLIO iterIn = Iter $ \c -> go $ (runIter iterIn) c
  where go (Done a (Chunk b c)) = Done a (Chunk b c)
        go (IterM m) = do
          let m1 = fmap go $ ioTCB m
          IterM m1
        go (IterF next) = IterF $ iterIOtoIterLIO next
        go (Fail itf ma mb) = Fail itf ma mb
        go _ = undefined -- TODO: complete implementation

onumIOtoOnumLIO :: LabelState l p s => Onum L IO L -> Onum L (LIO l p s) a
onumIOtoOnumLIO inO = mkInum $ iterIOtoIterLIO (inO .|$ dataI)
