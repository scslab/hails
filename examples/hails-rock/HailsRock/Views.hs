{-# LANGUAGE OverloadedStrings #-}

module HailsRock.Views where

import Prelude hiding (div, span, head, id)

import Data.Maybe (isJust, fromJust)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

import Text.Blaze.Html5 hiding (Tag, map)
import Text.Blaze.Html5.Attributes hiding ( label, form, span
                                          , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Control.Monad (forM_, when)

import Hails.Web hiding (body)
import Hails.HttpServer.Types

import HailsRock.MP


respondHtml :: Html -> Response
respondHtml content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title "HailsRock"
    meta ! charset "utf-8"
    link ! rel "stylesheet" 
         ! type_ "text/css" ! href "/static/css/bootstrap.css"
    script ! src "/static/js/jquery-1.10.1.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    script ! src "/static/js/application.js" $ ""
  body $ do
     div ! class_ "container-fluid" $ content

welcome :: Maybe UserName -> Html
welcome Nothing = do
  h1 $ "Welcome to HailsRock!"
  a ! class_ "btn btn-large btn-info"
    ! href "/login"
    $ "Login to play"
welcome (Just usr) = do
  h1 $ toHtml $ "Welcome to HailsRock, " ++  T.unpack usr ++  "!"
  a ! class_ "btn btn-large btn-primary"
               ! href "/game/new"
               $ "Create a new game"
  " "
  a ! class_ "btn btn-large"
               ! href "/game"
               $ "Join a game"

newGame :: UserName -> Html
newGame usr = do
  h1 $ "Create a new game"
  div $ do
    form ! action "/game/create" ! method "POST" ! id "newGame"$ do
      div $ do
        input ! type_ "hidden" ! name "creator"
              ! value (toValue usr)
      div $ do
        label ! for "opponent" $ "Opponent (optional):"
        input ! type_ "text"
              ! name "opponent" ! id "opponent" 
              ! placeholder "rick-james"
      div ! class_ "btn-group" $ do
        input ! type_ "submit" ! class_ "btn" ! value "Create"

listGames :: UserName -> [Game] -> Html
listGames usr gs' = do
  -- Get all the games for which the current user is not the creator;
  let gs = filter ((/= usr) . creator) gs'
  --
  h1 $ "Available games"
  div $ if null gs
    then p $ "Sorry, no games ... :-("
    else table ! class_ "table table-hover table-condensed" $ do
         thead $ tr $ do
           th $ "#"
           th $ "Creator"
           th $ "Private"
         tbody $ do
           forM_ (zip [1..] gs) $ \(nr,game) -> do
             let tagUrl = "/game/" ++ show (fromJust $ gameId game)
             tr ! onclick (toValue $ "location.href=" ++ show tagUrl )$ do
               td $ toHtml (nr :: Int)
               td $ toHtml $ creator game
               td $ when (isJust $ opponent game) $ "1-vs-1"

playGame :: UserName -> Game -> Bool -> Html
playGame usr game True = do
  h1 $ "You already played!"
playGame usr game False = do
  h1 $ "Make your move..."
  div $ do
    let gid = show . fromJust . gameId $ game
    form ! action (toValue $ "/game/"++gid++"/play") 
         ! method "POST" ! id "newGame"$ do
        input ! type_ "hidden" ! name "game"
              ! value (toValue gid)
        input ! type_ "hidden" ! name "player"
              ! value (toValue usr)
        input ! name "move"
              ! type_ "submit" 
              ! class_ "btn btn-large btn-info"
              ! value (toValue $ show Rock)
        " "
        input ! name "move"
              ! type_ "submit" 
              ! class_ "btn btn-large btn-primary"
              ! value (toValue $ show Paper)
        " "
        input ! name "move"
              ! type_ "submit" 
              ! class_ "btn btn-large btn-inverse"
              ! value (toValue $ show Scissors)

showStats :: [(UserName, Outcome)] -> Html
showStats stats = do
  h1 $ "Your move status"
  div $ if null stats
    then p $ "Sorry, nobody has played your move... :-("
    else table ! class_ "table table-hover table-condensed" $ do
         thead $ tr $ do
           th $ "#"
           th $ "Player"
           th $ "Status"
         tbody $ do
           forM_ (zip [1..] stats) $ \(nr,(p,result)) -> do
             tr $ do
               td $ toHtml (nr :: Int)
               td $ toHtml $ T.unpack p
               td $ toHtml $ show result
