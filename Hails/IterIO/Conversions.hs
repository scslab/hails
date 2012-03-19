{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
module Hails.IterIO.Conversions ( iterIOtoIterLIO
                                , ioIterRtoLIO
                                , onumIOtoOnumLIO
                                , inumIOtoInumLIO 
                                ) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef
import Data.IterIO
import LIO.DCLabel
import LIO.TCB
import Control.Monad
import Control.Monad.Trans

type L = L.ByteString

-- | Lift  the underlying monad of an 'Iter' from 'IO' to 'LIO'.
iterIOtoIterLIO :: (LabelState l p s, ChunkData t)
                => Iter t IO a -> Iter t (LIO l p s) a
iterIOtoIterLIO = adaptIterM rtioTCB

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


-- | Lift the underlying monad of the Iter and IterR result type.
inumResIOtoIO :: (LabelState l p s, ChunkData tIn, ChunkData tOut)
              => Iter tIn IO (IterR tOut IO a)
              -> Iter tIn (LIO l p s) (IterR tOut (LIO l p s) a)
inumResIOtoIO iIn = liftM ioIterRtoLIO $ iterIOtoIterLIO iIn

-- | Run an LIO computation.
iterLIOtoIterIO :: (LabelState l p s, ChunkData t)
                => Iter t (LIO l p s) a
                -> LIOstate l p s
                -> Iter t IO (a, LIOstate l p s)
iterLIOtoIterIO iter0 s0 = adaptIter (\a -> (a, s0)) adapt iter0
    where adapt m = lift (runLIO m s0) >>= uncurry iterLIOtoIterIO
-- iterLIOtoIterIO lio = runStateTLI (adaptIterM peelLIO lio)
--  where peelLIO (LIO x) = x


inumIOtoInumLIO :: (ChunkData tIn, ChunkData tOut)
    => Inum tIn tOut IO a
    -> LIOstate DCLabel TCBPriv ()
    -> Inum tIn tOut DC a
inumIOtoInumLIO io12 s0 = \dc1 -> do
  -- Create ref that will be used to store the final state after
  -- running dc1
  ref <- lift $ ioTCB $ newIORef s0 
  -- run dc1
  let io1 = do (x, s1) <- iterLIOtoIterIO dc1 s0 {- Iter tOut IO (a, LIOstate) -}
  -- save end state
               liftIO $ writeIORef ref s1
               return x
  -- apply io1 to the inum
      io2  = io12 io1                            {- Iter tIn  IO (IterR tOut IO a) -}
  -- change the monad from IO to DC
  res <- inumResIOtoIO io2                       {- Iter tIn  DC (IterR tOut DC a) -}
  -- set the end label to the end of that running dc1
  lift $ do s1 <- ioTCB $ readIORef ref
            putTCB s1
  return res
