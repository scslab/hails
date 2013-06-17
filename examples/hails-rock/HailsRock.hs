{-# LANGUAGE OverloadedStrings #-}

module HailsRock (server) where


import Data.Maybe
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad

import LIO
import LIO.DCLabel

import Hails.Data.Hson
import Hails.Database
import Hails.Database.Structured
import Hails.HttpServer
import Hails.Web
import qualified Hails.Web.Frank as Frank

import HailsRock.MP
import HailsRock.Views

import LIO.TCB

server :: Application
server = mkRouter $ do
  Frank.get "/" $ do
   musr <- getHailsUser
   return $ respondHtml $ welcome musr

  -- List games
  Frank.get "/game" $ withUserOrDoAuth $ \usr -> do
    lreq <- request 
    gs <- liftLIO . withHailsRockDB $ do
      games <- findAll $ select [] "games"
      plays' <- findAll $ select ["player" -: usr] "plays"
      let plays = map (Just . game) plays'
      return $ filter ((`notElem` plays) . gameId) games   
    return $ respondHtml $ listGames usr gs

  -- Create a new game view
  Frank.get "/game/new" $ withUserOrDoAuth $ \usr -> do
   return $ respondHtml $ newGame usr

  -- Create a new game
  Frank.post "/game/create" $ withUserOrDoAuth $ \usr -> do
    lreq <- request 
    liftLIO . withHailsRockDB $ do
      ldoc <- liftLIO $ labeledRequestToHson lreq
      _id <- insert "games" ldoc
      return $ redirectTo $ "/game/" ++ (show _id)

  -- Join a game
  Frank.get "/game/:id" $ withUserOrDoAuth $ \usr -> do
    (Just gid) <- queryParam "id"
    let _id = read . S8.unpack $ gid :: ObjectId
    (mgame, played) <- liftLIO . withHailsRockDB $ do
      mgame <- findBy "games" "_id" _id
      mplay <- findWhere $ select ["game" -: _id, "player" -: usr] "plays"
      return (mgame, isJust (mplay :: Maybe Play))
    return $ case mgame of
               Nothing -> forbidden
               Just game -> respondHtml $ playGame usr game played

  -- Make the move
  Frank.post "/game/:id/play" $ withUserOrDoAuth $ \usr -> do
    lreq <- request 
    (Just gid) <- queryParam "id"
    liftLIO . withHailsRockDB $ do
      ldoc <- liftLIO $ labeledRequestToHson lreq
      lrec <- fromLabeledDocument ldoc
      insertLabeledRecord (lrec :: DCLabeled Play)
      return $ redirectTo $ "/game/" ++ (S8.unpack gid) ++ "/status"

  -- Get status on wins
  Frank.get "/game/:id/status" $ withUserOrDoAuth $ \usr -> do
    lreq <- request 
    (Just gid) <- queryParam "id"
    let _id = read . S8.unpack $ gid :: ObjectId
    mplay <- liftLIO . withHailsRockDB $ do
      findWhere $ select ["game" -: _id, "player" -: usr] "plays"
    case mplay of
      Nothing -> return notFound
      Just play -> do liftLIO $ ioTCB $ putStrLn $ "GET HERE"
                      stats <- liftLIO $ getStats play
                      liftLIO $ ioTCB $ putStrLn $ "stats = " ++ show stats
                      return $ respondHtml $ showStats stats

  -- Enable login
  Frank.get "/login" $ withUserOrDoAuth $ 
    const (return $ redirectTo "/")

