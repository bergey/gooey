{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Render

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
  state <- newMVar ()

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
animate :: VMount -> Chan () -> MVar () -> IO ()
animate m q sVar = do
  s <- readMVar sVar
  p <- diff m (render (writeChan q) s)
  void $ inAnimationFrame ContinueAsync $ patch m p >> animate m q sVar

update :: () -> () -> ()
update = const id

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Chan e -> (e -> s -> s) -> IO ()
queueHandler s q h = do
  ev <- readChan q
  modifyMVar_ s (return . h ev)
  queueHandler s q h
