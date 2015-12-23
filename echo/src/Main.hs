-- |

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLInputElement (getValue)
import           GHCJS.DOM.Types            hiding (Event)

import           Control.Concurrent
import           Control.Monad              hiding (sequence_)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body $ Just initialHtml
  Just inElem <- fmap castToHTMLInputElement <$> getElementById doc "in"
  Just out <- getElementById doc "out"
  forever $ do
    Just val <- getValue inElem :: IO (Maybe String)
    setInnerHTML out $ Just val
    threadDelay 500000

initialHtml :: String
initialHtml = "<input id=\"in\"></input><p id=\"out\"></p>"
