{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Orphans
import           Render
import           Types

import qualified Data.JSString                 as S
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement
import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import qualified GHCJS.VDOM.Element            as E
import           JavaScript.Web.AnimationFrame

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Semigroup                ((<>))

main :: IO ()
main = do
  actionQueue <- newChan
  state <- newMVar $ initialState

  -- mount VDom
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  -- render in a loop
  animate m actionQueue state

  -- fork event handler
  void $ forkIO $ queueHandler state actionQueue update

-- It would be easy to factor out State from this, and take @render@
-- as an argument.
animate :: VMount -> Chan Action -> MVar State -> IO ()
animate m q sVar = do
  s <- readMVar sVar
  p <- diff m (render (writeChan q) s)
  void $ inAnimationFrame ContinueAsync $ patch m p >> animate m q sVar

update :: Action -> State -> State
update NoOp state = state
update (UpdateField str) state = state { field = str }
update Add state = let desc = trim (field state) in
  if desc == "" then state else state
                                { uid = uid state + 1
                                , field = ""
                                , tasks = tasks state ++ [mkTask desc (uid state)]
                                }
update (UpdateTask i a) state = let
  f t = if taskId t == i then updateTask a t else Just t
  in
    state { tasks = catMaybes . fmap f $ tasks state}
update DeleteComplete state =
  state { tasks = filter ( not . completed ) $ tasks state }
update (CheckAll ch) state = let
  f t = t { completed = ch }
  in
    state { tasks = fmap f $ tasks state }
update (ChangeVisibility vis) state = state { visibility = vis }

updateTask :: TaskAction -> Task -> Maybe Task
updateTask Delete _ = Nothing
updateTask Focus task = Just $ task { edits = Just $ description task }
updateTask (Edit k) task = let
  edits' = fromMaybe "" $ edits task
    in
    Just $ task { edits = Just $ edits' <> k }
updateTask Cancel task = Just $ task { edits = Nothing }
updateTask Commit task = case edits task of
  Nothing -> Just task
  Just raw -> let
    desc = trim raw
    in
      if desc == "" then Nothing
      else Just $ task
           { edits = Nothing
           , description = desc
           }
updateTask (Complete c) task = Just $ task { completed = c }

-- utility functions, not specific to this application

trim :: S.JSString -> S.JSString
trim = S.dropWhile isSpace . S.dropWhileEnd isSpace

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Chan e -> (e -> s -> s) -> IO ()
queueHandler s q h = do
  ev <- readChan q
  modifyMVar_ s (return . h ev)
  queueHandler s q h
