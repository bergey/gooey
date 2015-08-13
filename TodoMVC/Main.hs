{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Queue
import           Render
import           Types

import Data.Char
import Data.List
import           Control.Concurrent
import           Control.Concurrent.MVar

main :: IO ()
main = do
  actionQueue <- newQueue
  state <- newMVar $ initialState

  -- mount VDom
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  -- render in a loop
  animate m state

  -- fork event handler
  void $ forkIO $ queueHandler state actionQueue update

-- It would be easy to factor out State from this, and take @render@
-- as an argument.
animate :: VMount -> MVar State -> IO ()
animate m sVar = do
  s <- readMVar sVar
  p <- diff m (render s)
  void $ inAnimationFrame ContinueAsync $ patch m p >> animate m sVar

update :: Action -> State -> State
update action state = case action of
  NoOp -> state
  UpdateField str -> model { field = str }
  Add -> let desc = trim (field state) in
  UpdateTask i a = let up t = if (taskId )


trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
