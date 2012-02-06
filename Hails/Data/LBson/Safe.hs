{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- | This module exports a safe subset of the labeled BSON (LBSON)
   module. See "Hails.Data.LBson.TCB" for documentation.
-}

module Hails.Data.LBson.Safe ( -- * UTF-8 String
                               module Data.UString
                               -- * Document
                             , Document, LabeledDocument
                             , look, lookup, valueAt, at, include, exclude, merge
                               -- * Field
                             , Field(..), (=:), (=?)
                             , Key
                               -- * Value
                             , Value, val, cast', cast, typed
                               -- * Policy labeled values
                             , pu, pl
                               -- * Special Bson value types
                             , Binary(..)
                             , Function(..)
                             , UUID(..)
                             , MD5(..)
                             , UserDefined(..)
                             , Regex(..)
                             , Javascript(..)
                             , Symbol(..)
                             , MongoStamp(..)
                             , MinMaxKey(..)
                               -- ** ObjectId
                             , ObjectId(..)
                             , timestamp
                             , genObjectId
                             ) where
import Prelude ()
import Hails.Data.LBson.TCB
import Data.UString
