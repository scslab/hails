{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Hails.Web.Controller
  ( Controller
  , request
  , body
  , queryParam
  , respond
  ) where

import LIO
import LIO.DCLabel

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
--import Network.Wai.Parse
import Hails.HttpServer
import Hails.Web.Router

data ControllerState = ControllerState { csRequest :: DCLabeled Request }

type Controller = ReaderT ControllerState DC

instance MonadLIO DCLabel Controller where
  liftLIO = lift

instance Routeable (Controller Response) where
  runRoute controller _ _ req = fmap Just $
    runReaderT controller $ ControllerState req

request :: Controller (DCLabeled Request)
request = fmap csRequest ask

queryParam :: S8.ByteString -> Controller (Maybe S8.ByteString)
queryParam varName = do
  req <- request >>= liftLIO . unlabel
  let qr = queryString req
  case lookup varName qr of
    Just n -> return n
    _ -> return Nothing

respond :: Routeable r => r -> Controller r
respond = return

body :: Controller L8.ByteString
body = request >>= liftLIO . unlabel >>= return . requestBody

