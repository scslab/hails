module Hails.IterIO.Conversions (iterIOtoIterDC, onumIOtoOnumDC) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import qualified LIO.TCB as LIO
import LIO.DCLabel

type L = L.ByteString

iterIOtoIterDC :: ChunkData a => Iter a IO b -> Iter a DC b
iterIOtoIterDC iterIn = Iter $ \c -> go $ (runIter iterIn) c
  where go (Done a (Chunk b c)) = Done a (Chunk b c)
        go (IterM m) = do
          let m1 = fmap go $ LIO.ioTCB m
          IterM m1
        go (IterF next) = IterF $ iterIOtoIterDC next
        go (Fail itf ma mb) = Fail itf ma mb
        go _ = undefined -- TODO: complete implementation

onumIOtoOnumDC :: Onum L IO L -> Onum L DC a
onumIOtoOnumDC inO = mkInum $ iterIOtoIterDC (inO .|$ dataI)
