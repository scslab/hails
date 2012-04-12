{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

-- | Moduule rexport Bson Value value-constructors
module Hails.Data.LBson.Rexports( Value ( Float
                                        , String
                                        , Doc
                                        , Array
                                        , Bin
                                        , Fun
                                        , Uuid
                                        , Md5
                                        , UserDef
                                        , ObjId
                                        , Bool
                                        , UTC
                                        , Null
                                        , RegEx
                                        , JavaScr
                                        , Sym
                                        , Int32
                                        , Int64
                                        , Stamp
                                        , MinMax )
                                ) where
import Data.Bson
