{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             OverloadedStrings #-}

module Main (main) where

import Data.Typeable
import Data.Text ()

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

data UsersPolicyModule = UsersPolicyModuleTCB DCPriv
  deriving Typeable

instance PolicyModule UsersPolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> anybody
        writers ==> anybody
        admins  ==> this
      collection "users" $ do
        access $ do
          readers ==> anybody
          writers ==> anybody
        clearance $ do
          secrecy   ==> this
          integrity ==> anybody
        document $ \_ -> do
          readers ==> anybody
          writers ==> anybody
        field "name"     $ searchable
        field "password" $ labeled $ \doc -> do
          readers ==> this \/ ("name" `at` doc :: String)
          writers ==> this \/ ("name" `at` doc :: String)
    return $ UsersPolicyModuleTCB priv
      where this = privDesc priv

withUsersPolicyModule :: DBAction a -> DC a
withUsersPolicyModule act = withPolicyModule (\(_ :: UsersPolicyModule) -> act)


-- | Create databse config file
mkDBConfFile :: IO ()
mkDBConfFile = do
  writeFile dbConfFile (unlines [show pm])
  setEnv "DATABASE_CONFIG_FILE" dbConfFile False
   where pm :: (String, String, String)
         pm = (mkName (UsersPolicyModuleTCB undefined), "_users", "users_db")
         dbConfFile = "/tmp/hails_example_database.conf"
         mkName x =
            let tp = typeRepTyCon $ typeOf x
            in tyConPackage tp ++ ":" ++ tyConModule tp ++ "." ++ tyConName tp

main :: IO ()
main = do
  mkDBConfFile
  withUser "alice" app1
  withUser "bob"   (app2 False)
  withUser "bob"   (app2 True)
  withUser "alice" (app2 True)
   where withUser :: String -> (String -> DCPriv -> DC ()) -> IO ()
         withUser u act = putStrLn . show =<< (paranoidDC $ do
           let prin = toComponent u
           setClearanceP allPrivTCB (dcLabel prin dcTrue)
           act u $ mintTCB prin)

app1 :: String -> DCPriv -> DC ()
app1 usr priv = do
  let p = toBsonValue ("w00tw00t" :: String)
  withUsersPolicyModule $ do
    let doc :: HsonDocument
        doc = [ "name" -: usr, "password" -: needPolicy p]
    insertP_ priv "users" doc

app2 :: Bool -> String -> DCPriv -> DC ()
app2 readPass _ priv = do
  ldocs <- withUsersPolicyModule $ do
            cur <-findP priv $ select [] "users"
            getAll [] cur
  --
  forM_ ldocs $ \ldoc -> do
    doc <- unlabelP priv ldoc
    putStrLn' $ "name = " ++ ("name" `at` doc)
    when readPass $ do
      lpass <- getPolicyLabeled ("password" `at` doc)
      pass <- unlabelP priv lpass
      putStrLn' $ "password = " ++ show pass
  where getAll acc cur = do
          mldoc <- nextP priv cur
          case mldoc of
            Nothing -> return acc
            Just ldoc -> getAll (ldoc:acc) cur

putStrLn' :: String -> DC ()
putStrLn' m = ioTCB $ putStrLn m
