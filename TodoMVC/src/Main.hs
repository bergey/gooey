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
  q <- newChan
  state <- newMVar ()
  -- loop, push to Chan
  void . forkIO . forever $ do
    writeChan q ()
    threadDelay 1000000
  animate  -- requestAnimationFrame loop
  -- read, read from Chan
  void . forkIO . forever $ readChan q

animate :: IO ()
animate = void $ inAnimationFrame ContinueAsync animate
