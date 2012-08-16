{-# LANGUAGE Unsafe #-}

{- |

This module exports the trusted types and functions used by
"Hails.Database.Query" when performing database queries.

-}

module Hails.Database.Query.TCB ( 
  -- * Labeled cursor
    Cursor(..)
  ) where

import           LIO.DCLabel
import           Hails.Data.Hson
import           Hails.Database.TCB
import qualified Database.MongoDB as Mongo

-- | A labeled cursor. The cursor is labeled with the join of the
-- database and collection it reads from. The collection policies
-- are \"carried\" along since they are applied on-demand.
data Cursor = CursorTCB { curLabel     :: DCLabel
                        -- ^ Cursor label
                        , curInternal  :: Mongo.Cursor
                        -- ^ Internal MongoDB cursor
                        , curProject   :: [FieldName]
                        -- ^ Projector from query. Used to remove
                        -- fields after performing query.
                        , curCollection:: Collection
                        -- ^ Collection cursor is reading from
                        } 
