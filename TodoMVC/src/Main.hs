{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           GHCJS.Foreign.Callback        (OnBlocked (..))
import           JavaScript.Web.AnimationFrame (inAnimationFrame)

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                 (forever, void)

main :: IO ()
main = do
  actionQueue <- newChan
  state <- newMVar ()
  -- loop, push to Chan
  void . forkIO . forever $ do
    writeChan actionQueue ()
    threadDelay 1000000
  animate  -- requestAnimationFrame loop
  -- fork event handler
  void $ forkIO $ queueHandler state actionQueue

animate :: IO ()
animate = void $ inAnimationFrame ContinueAsync animate

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Chan e -> IO ()
queueHandler s q = do
  ev <- readChan q
  modifyMVar_ s return
  queueHandler s q
