{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
module Hails.IterIO.Conversions ( iterIOtoIterLIO
                                , ioIterRtoLIO
                                , onumIOtoOnumLIO
                                ) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import LIO.TCB

type L = L.ByteString

-- | Lift  the underlying monad of an 'Iter' from 'IO' to 'LIO'.
iterIOtoIterLIO :: (LabelState l p s, ChunkData t)
                => Iter t IO a -> Iter t (LIO l p s) a
iterIOtoIterLIO iterIn = Iter $ \c -> ioIterRtoLIO $ (runIter iterIn) c

-- | Lift  the underlying monad of an 'IterR' from 'IO' to 'LIO'.
ioIterRtoLIO :: (LabelState l p s, ChunkData t)
             => IterR t IO a -> IterR t (LIO l p s) a
ioIterRtoLIO (Done a (Chunk b c)) = Done a (Chunk b c)
ioIterRtoLIO (IterM m) = IterM $ fmap ioIterRtoLIO $ rtioTCB m
ioIterRtoLIO (IterF next) = IterF $ iterIOtoIterLIO next
ioIterRtoLIO (Fail itf ma mb) = Fail itf ma mb
ioIterRtoLIO (IterC (CtlArg carg f c)) =
  let g = \cres -> iterIOtoIterLIO $ f cres
  in IterC (CtlArg carg g c)

-- | Lift the underlying monad of an 'Inum' from 'IO' to 'LIO'.
onumIOtoOnumLIO :: LabelState l p s => Onum L IO L -> Onum L (LIO l p s) a
onumIOtoOnumLIO inO = mkInum $ iterIOtoIterLIO (inO .| dataI)
