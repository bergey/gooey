-- | Hello World for GHCJS using ghcjs-dom.

module Main where

import GHCJS.DOM.HTMLElement
import GHCJS.DOM
import GHCJS.DOM.Document

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body "<p/>Hello World</p>"
