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
  htmlElementSetInnerHTML body $ initialHtml
  Just input <- fmap castToHTMLInputElement <$> documentGetElementById doc "in"
  Just out <- fmap castToHTMLElement <$> documentGetElementById doc "out"
  forever $ do
    val <- htmlInputElementGetValue input :: IO String
    htmlElementSetInnerText out val
    threadDelay 500000

initialHtml :: String
initialHtml = "<input id=\"in\"></input><p id=\"out\"></p>"
