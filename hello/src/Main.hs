-- | Hello World for GHCJS using ghcjs-dom.

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLElement

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body "<p/>Hello World</p>"
