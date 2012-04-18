{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

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

getZonedTime :: (LabelState l p s) => LIO l p s ZonedTime
getZonedTime = rtioTCB T.getZonedTime

utcToLocalZonedTime :: (LabelState l p s) => UTCTime -> LIO l p s ZonedTime
utcToLocalZonedTime = rtioTCB . T.utcToLocalZonedTime