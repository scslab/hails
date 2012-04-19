{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- | This module re-exports "Data.Time" wrapped in 'LIO'. It is
important to note that this module is only safe with the latest
version of "LIO", where @toLabeled@ has been removed and timing
attacked have been addressed. In similar vain, when executing a
piece of code that you do not trust, it is important that the time
primitives not be directly available. -}
module LIO.Data.Time ( module Data.Time
                       , getCurrentTime
                       , getZonedTime
                       , utcToLocalZonedTime) where

import qualified Data.Time as T
import Data.Time hiding ( getCurrentTime
                        , getZonedTime
                        , utcToLocalZonedTime)
import LIO.TCB

-- | Get the current UTC time from the system clock.
getCurrentTime :: (LabelState l p s) => LIO l p s UTCTime
getCurrentTime = rtioTCB T.getCurrentTime

-- | Get the local time together with a TimeZone.
getZonedTime :: (LabelState l p s) => LIO l p s ZonedTime
getZonedTime = rtioTCB T.getZonedTime

-- | Convert UTC time to local time with TimzeZone
utcToLocalZonedTime :: (LabelState l p s) => UTCTime -> LIO l p s ZonedTime
utcToLocalZonedTime = rtioTCB . T.utcToLocalZonedTime