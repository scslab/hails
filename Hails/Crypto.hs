{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | This module exports various cryptographic primitives.
module Hails.Crypto ( -- * Hash functions
                      module Data.Digest.Pure.SHA
                    , module Data.Digest.Pure.MD5
                      -- * Symmetric ciphers
                    , module Codec.Crypto.SimpleAES
                      -- * Asymmetric ciphers
                    , module Codec.Crypto.RSA
                    ) where
import Data.Digest.Pure.SHA
import Data.Digest.Pure.MD5
import Codec.Crypto.SimpleAES
import Codec.Crypto.RSA
