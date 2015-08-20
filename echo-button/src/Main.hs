-- |

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLInputElement    (htmlInputElementGetValue)
import           GHCJS.DOM.Types               hiding (Event)
import           GHCJS.Foreign
import           GHCJS.Marshal

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad                 hiding (sequence_)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body initialHtml
  Just input <- fmap castToHTMLInputElement <$> documentGetElementById doc "in"
  Just out <- fmap castToHTMLElement <$> documentGetElementById doc "out"
  Just button <- documentGetElementById doc "button"
  -- cb <- eventListenerNew (echo input out)
  void $ eventTargetAddEventListener button "click" False $ echo input out

initialHtml :: String
initialHtml = "<input id=\"in\"></input><p id=\"out\"></p><div id=\"button\" style=\"color: green; border:1px solid green;\">echo</>"

echo :: HTMLInputElement -> HTMLElement -> Element -> MouseEvent -> IO ()
echo input out _self _ev = do
  val <- htmlInputElementGetValue input :: IO String
  htmlElementSetInnerText out val
  threadDelay 500000
