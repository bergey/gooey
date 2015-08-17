{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           GHCJS.Foreign.Callback        (OnBlocked (..))
import           GHCJS.Foreign.QQ              (js, js_)
import           JavaScript.Web.AnimationFrame (inAnimationFrame)

import           GHCJS.VDOM
import qualified GHCJS.VDOM.Element            as E
import           GHCJS.VDOM.Event              (keypress)

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                 (void)

main :: IO ()
main = do
  actionQueue <- newChan
  state <- newMVar ()

  -- mount VDom
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  -- render in a loop
  animate m actionQueue state
  -- fork event handler
  void $ forkIO $ queueHandler state actionQueue

animate :: VMount -> Chan () -> MVar () -> IO ()
animate m q sVar = do
  s <- readMVar sVar
  p <- diff m (render (writeChan q ()))
  void $ inAnimationFrame ContinueAsync $ patch m p >> animate m q sVar

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Chan e -> IO ()
queueHandler s q = do
  ev <- readChan q
  modifyMVar_ s return
  queueHandler s q

render :: (IO ()) -> VNode
render raise = E.input [keypress (const raise)] ()
