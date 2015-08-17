-- |

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLInputElement    (getValue)
import           GHCJS.DOM.Types               hiding (Event)
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Web.AnimationFrame

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad                 hiding (sequence_)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body $ Just initialHtml
  Just input <- fmap castToHTMLInputElement <$> getElementById doc "in"
  Just out <- fmap castToHTMLElement <$> getElementById doc "out"
  Just button <- getElementById doc "button"
  cb <- eventListenerNew (echo input out)
  addEventListener button "click" (Just cb) False

initialHtml :: String
initialHtml = "<input id=\"in\"></input><p id=\"out\"></p><div id=\"button\" style=\"color: green; border:1px solid green;\">echo</>"

echo :: HTMLInputElement -> HTMLElement -> MouseEvent -> IO ()
echo input out _ev = do
  val <- getValue input :: IO (Maybe String)
  setInnerText out val
  threadDelay 500000
