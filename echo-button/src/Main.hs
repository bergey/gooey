-- |

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLInputElement    (getValue)
import           GHCJS.DOM.Types               hiding (Event)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body $ Just initialHtml
  Just inElem <- fmap castToHTMLInputElement <$> getElementById doc "in"
  Just out <- getElementById doc "out"
  Just button <- getElementById doc "button"
  cb <- eventListenerNew $ echo inElem out
  addEventListener button "click" (Just cb) False

initialHtml :: String
initialHtml = "<input id=\"in\"></input><p id=\"out\"></p><div id=\"button\" style=\"color: green; border:1px solid green;\">echo</>"

echo :: HTMLInputElement -> Element -> MouseEvent -> IO ()
echo inElem out _ev = do
  Just val <- getValue inElem :: IO (Maybe String)
  setInnerHTML out $ Just val
  -- threadDelay 500000
