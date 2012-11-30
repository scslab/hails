{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

This module exports a type corresponding to user\'s in Hails
and some helper functions.

-}
module Hails.Web.User (
    UserName
  , getHailsUser
  , withUserOrDoAuth
  ) where


import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import           Data.Text (Text)
import           Hails.Web.Controller
import           Hails.HttpServer

-- | User name.
type UserName = Text

-- | Execute action with the current user's name. Otherwise, request
-- that the user authenticate.
withUserOrDoAuth :: (UserName -> Controller Response)
                 -> Controller Response
withUserOrDoAuth act = getHailsUser >>= \muser ->
  maybe (return reqLogin) act muser
    where reqLogin = Response status200 [("X-Hails-Login", "Yes")] ""

-- | Get the current user.
getHailsUser :: Controller (Maybe UserName)
getHailsUser = do
  fmap (fmap (T.pack . S8.unpack)) $ requestHeader "x-hails-user"


