{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module HailsRock.MP ( HailsRockModule 
                    , withHailsRockDB
                    , Game(..)
                    , Play(..)
                    , Move(..)
                    , Outcome(..)
                    , getStats
                    ) where

import Prelude hiding (lookup)

import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
import Data.Typeable
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

import Control.Monad

import LIO
import LIO.DCLabel

import Hails.Data.Hson
import Hails.Database
import Hails.Database.Structured
import Hails.PolicyModule
import Hails.PolicyModule.DSL
import Hails.Web

-- | A game contains can be public or contain a single opponent
data Game = Game { gameId   :: Maybe ObjectId
                 , creator  :: UserName
                 , opponent :: Maybe UserName } deriving (Show)

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

-- | A play is a move played by a user for a game
data Play = Play { playId :: Maybe ObjectId
                 , game   :: ObjectId
                 , player :: UserName
                 , move   :: Move } deriving (Show)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
outcome _ _                  = Lose

instance DCRecord Game where
  fromDocument doc = do
    let _id = lookupObjId "_id" doc
    p1 <- lookup "creator" doc
    -- Ignore empty-string opponents
    let p2 = do op <- lookup "opponent" doc
                let u = T.unwords . T.words $ op
                if T.null u
                  then fail "Null opponent"
                  else return u
    return Game { gameId   = _id
                , creator  = p1
                , opponent = p2 }
                
  toDocument game = 
    let emptyOr f m = maybe [] (\i -> [f -: i]) m
        _id = emptyOr "_id" $ gameId game
        opp = emptyOr "opponent" $ opponent game
    in _id ++ [ "creator"  -: creator game ] ++ opp

  recordCollection _ = "games"

instance DCLabeledRecord HailsRockModule Game where
  endorseInstance _ = HailsRockModuleTCB mempty

instance DCRecord Play where
  fromDocument doc = do
    let _id = lookupObjId "_id" doc
    gid <- lookupObjId "game" doc
    p   <- lookup "player" doc
    mv  <- lookupMove "move" doc
    return Play { playId = _id
                , game   = gid
                , player = p
                , move   = mv }
                
  toDocument play = 
    let emptyOr f m = maybe [] (\i -> [f -: i]) m
        _id = emptyOr "_id" $ playId play
    in _id ++ [ "game"   -: game play
              , "player" -: player play
              , "move"   -: (show $ move play) ]

  recordCollection _ = "plays"

instance DCLabeledRecord HailsRockModule Play where
  endorseInstance _ = HailsRockModuleTCB mempty

--
--
--


data HailsRockModule = HailsRockModuleTCB DCPriv
  deriving Typeable

instance PolicyModule HailsRockModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> unrestricted
        writers ==> unrestricted
        admins  ==> this
      --
      collection "games" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy   ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          let (Just game) = fromDocument doc
          readers ==> unrestricted
          writers ==> this \/ (userToPrincipal $ creator game)
      --
      collection "plays" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy   ==> this
          integrity ==> unrestricted
        document $ \doc -> do
          let (Just play) = fromDocument doc
          readers ==> this \/ (userToPrincipal $ player play)
          writers ==> this \/ (userToPrincipal $ player play)
        field "game" key
        field "player" key
    return $ HailsRockModuleTCB priv
      where this = privDesc priv
            userToPrincipal = principal . T.unpack

withHailsRockDB :: DBAction a -> DC a
withHailsRockDB act = withPolicyModule (\(_ :: HailsRockModule) -> act)

--
-- Sensitive getStats function
--

getStats :: Play -> DC [(UserName, Outcome)]
getStats playA = withPolicyModule $ \(HailsRockModuleTCB priv) -> do
  ps <- findAllPlays $ game playA
  stats <- forM ps $ \lplay -> do
    playB <- liftLIO $ unlabelP priv lplay
    return $ (player playB, outcome (move playA) (move playB))
  return $ filter ((/= player playA) . fst) stats
    
  


--
-- Helpers
--

findAllPlays :: ObjectId -> DBAction [DCLabeled Play]
findAllPlays _id = do
  cursor <- find $ select ["game" -: _id] "plays"
  cursorToRecords cursor []
  where cursorToRecords cur docs = do
          mldoc <- next cur
          case mldoc of
            Just ldoc -> do
                d <- fromLabeledDocument ldoc
                cursorToRecords cur $ d:docs
            _ -> return $ reverse docs

-- | Generic lookup with possible type cast
lookupTyped :: (HsonVal a, Read a, Monad m) => FieldName -> HsonDocument -> m a
lookupTyped n d = case lookup n d of
    Just i -> return i
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupTyped: cannot extract id from " ++ show n


-- | Get object id (may need to convert from string).
lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId = lookupTyped 

-- | Get move (may need to convert from string).
lookupMove :: Monad m => FieldName -> HsonDocument -> m Move
lookupMove f d = do
  s <- lookupTyped f d
  maybe (fail "lookupMove failed to find move") return $  maybeRead s
    
-- | Try to read a value
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
