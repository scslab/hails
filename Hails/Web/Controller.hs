{-# LANGUAGE OverloadedStrings
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleInstances
           , MultiParamTypeClasses #-}

{- | 
This module exports a definition of a 'Controller', which is simply a
'DC' action with the 'Labeled' HTTP 'Request' in the environment
(i.e., it is a 'Reader' monad).
-}

module Hails.Web.Controller
  ( Controller
  , request
  , requestHeader 
  , body
  , queryParam
  , respond
  , redirectBack
  , redirectBackOr 
  ) where

import           LIO
import           LIO.DCLabel

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import           Network.HTTP.Types.Header
import           Hails.HttpServer
import           Hails.Web.Router
import           Hails.Web.Responses

data ControllerState = ControllerState { csRequest :: DCLabeled Request }

-- | A controller is simply a reader monad atop 'DC' with the 'Labeled'
-- 'Request' as the environment.
type Controller = ReaderT ControllerState DC

instance MonadLIO DCLabel Controller where
  liftLIO = lift

instance Routeable (Controller Response) where
  runRoute controller _ _ req = fmap Just $
    runReaderT controller $ ControllerState req

-- | Get the underlying request.
request :: Controller (DCLabeled Request)
request = fmap csRequest ask

-- | Get the query parameter mathing the supplied variable name.
queryParam :: S8.ByteString -> Controller (Maybe S8.ByteString)
queryParam varName = do
  req <- request >>= liftLIO . unlabel
  let qr = queryString req
  case lookup varName qr of
    Just n -> return n
    _ -> return Nothing

-- | Produce a response.
respond :: Routeable r => r -> Controller r
respond = return

-- | Extract the body in the request (after unlabeling it).
body :: Controller L8.ByteString
body = request >>= liftLIO . unlabel >>= return . requestBody

-- | Get a request header
requestHeader :: HeaderName -> Controller (Maybe S8.ByteString)
requestHeader name = do
  req <- request >>= liftLIO . unlabel
  return $ lookup name $ requestHeaders req

-- | Redirect back acording to the referer header. If the header is
-- not present redirect to root (i.e., @\/@).
redirectBack :: Controller Response
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back acording to the referer header. If the header is
-- not present return the given response.
redirectBackOr :: Response -> Controller Response
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  return $ case mrefr of
    Just refr -> redirectTo $ S8.unpack refr
    Nothing   -> def
