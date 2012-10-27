{- |

This module re-exports the routing and controller modules.
See each module for their corresponding documentation.
  
Though you can implement a controller using the methods supplied by
this module (actually, "Hails.Web.Router"), we recommend using the
DSLs provided by "Hails.Web.Frank" or "Hails.Web.REST".

-}
module Hails.Web (
    module Hails.Web.Router
  , module Hails.Web.Responses
  , module Hails.Web.Controller
  , module Hails.Web.User
  ) where

import Hails.Web.Router
import Hails.Web.Responses
import Hails.Web.Controller
import Hails.Web.User
