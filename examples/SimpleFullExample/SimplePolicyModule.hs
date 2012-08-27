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

import LIO.TCB (ioTCB)
import LIO.Privs.TCB (mintTCB)
import LIO.DCLabel.Privs.TCB (allPrivTCB)
import System.Posix.Env (setEnv)

data StorePolicyModule = StorePolicyModuleTCB DCPriv
  deriving Typeable

instance PolicyModule StorePolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> anybody
        writers ==> anybody
        admins  ==> this
      collection "store" $ do
        access $ do
          readers ==> anybody
          writers ==> anybody
        clearance $ do
          secrecy   ==> this
          integrity ==> anybody
        document $ \_ -> do
          readers ==> anybody
          writers ==> anybody
        field "key" key
    return $ StorePolicyModuleTCB priv
      where this = privDesc priv

withStorePolicyModule :: DBAction a -> DC a
withStorePolicyModule act = withPolicyModule (\(_ :: StorePolicyModule) -> act)

