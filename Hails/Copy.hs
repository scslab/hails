module Hails.Copy where

import LIO.SafeCopy

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Network.HTTP.Types
import qualified Data.CaseInsensitive
import Network.Socket
import Data.Time.Clock
import Data.Time.Calendar
import Data.Bson

import Control.Exception
import Control.Applicative

lim = 30000

-- Unbounded ops, hope you know what you're doing!

tr = transfer

trByteString :: Transfer Data.ByteString.ByteString
trByteString = Transfer $ \bs -> evaluate (Data.ByteString.copy bs)

trS8ByteString :: Transfer Data.ByteString.Char8.ByteString
trS8ByteString = Transfer $ \bs -> evaluate (Data.ByteString.Char8.copy bs)

trLByteString :: Transfer Data.ByteString.Lazy.ByteString
trLByteString = Transfer $ \bs -> evaluate (Data.ByteString.Lazy.copy bs)

trText :: Transfer Data.Text.Text
trText = Transfer $ \t -> evaluate (Data.Text.copy t)

trCIByteString :: Data.CaseInsensitive.FoldCase s => Transfer s -> Transfer (Data.CaseInsensitive.CI s)
trCIByteString t = Transfer $ \x -> Data.CaseInsensitive.mk <$> transfer t (Data.CaseInsensitive.original x)

trSockAddr :: Transfer SockAddr
trSockAddr = Transfer f
    where f (SockAddrInet a b) = SockAddrInet <$> transfer trPortNumber a <*> transfer trPrim b
          f (SockAddrInet6 a b (c1, c2, c3, c4) d) = SockAddrInet6 <$> transfer trPortNumber a <*> transfer trPrim b <*> ((,,,) <$> tr trPrim c1 <*> tr trPrim c2 <*> tr trPrim c3 <*> tr trPrim c4) <*> transfer trPrim d
          f (SockAddrUnix s) = SockAddrUnix <$> transfer (trList lim trPrim) s

trPortNumber :: Transfer PortNumber
trPortNumber = Transfer $ \(PortNum n) -> PortNum <$> tr trPrim n

trMaybe :: Transfer a -> Transfer (Maybe a)
trMaybe t = Transfer f
    where f Nothing = return Nothing
          f (Just x) = Just <$> transfer t x

trUTCTime :: Transfer UTCTime
trUTCTime = Transfer $ \(UTCTime d dt) -> UTCTime <$> transfer trDay d <*> transfer trDiffTime dt

-- Probably wrong, but...
trInteger :: Transfer Integer
trInteger = Transfer copy

trDay :: Transfer Day
trDay = Transfer $ \(ModifiedJulianDay d) -> ModifiedJulianDay <$> transfer trInteger d

trDiffTime :: Transfer DiffTime -- TERRIBLE
trDiffTime = Transfer $ \dt -> evaluate (picosecondsToDiffTime (truncate (toRational dt * 1000000000000)))

trHttpVersion :: Transfer Network.HTTP.Types.HttpVersion
trHttpVersion = Transfer $ \(Network.HTTP.Types.HttpVersion a b) -> Network.HTTP.Types.HttpVersion <$> transfer trPrim a <*> transfer trPrim b

trObjectId :: Transfer ObjectId
trObjectId = Transfer $ \(Oid a b) -> Oid <$> transfer trPrim a <*> transfer trPrim b
