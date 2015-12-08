-- | Hello World for GHCJS using ghcjs-dom.

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body $ Just "<p/>Hello World</p>"
