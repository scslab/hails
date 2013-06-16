{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             OverloadedStrings #-}

module SimplePolicyModule (
    StorePolicyModule 
  , withStorePolicyModule
  ) where

import Data.Typeable

import Control.Monad

import LIO
import LIO.DCLabel
import Hails.Data.Hson
import Hails.Database
import Hails.PolicyModule
import Hails.PolicyModule.DSL

import System.Posix.Env (setEnv)

data StorePolicyModule = StorePolicyModuleTCB DCPriv
  deriving Typeable

instance PolicyModule StorePolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> unrestricted
        writers ==> unrestricted
        admins  ==> this
      collection "store" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy   ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "key" key
    return $ StorePolicyModuleTCB priv
      where this = privDesc priv

withStorePolicyModule :: DBAction a -> DC a
withStorePolicyModule act = withPolicyModule (\(_ :: StorePolicyModule) -> act)

