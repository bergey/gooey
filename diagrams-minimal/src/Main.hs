{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Diagrams.Backend.GHCJS
import           Diagrams.Prelude
import qualified JavaScript.Web.Canvas          as C

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Types                hiding (Event)
import           GHCJS.Types
import qualified JavaScript.Web.Canvas.Internal as C

import           Data.Coerce
import qualified Data.Text                      as T

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body $ Just initialHtml
  ctx <- getContextById "dia"
  renderDia Canvas (CanvasOptions (dims2D 200 200) ctx) dia

getContextById :: JSString -> IO C.Context
getContextById name = do
  Just doc <- currentDocument
  Just c <- getElementById doc name
  C.getContext . coerce $ c

initialHtml :: T.Text
initialHtml = "<canvas id=\"dia\" width=\"200\" height=\"200\"></canvas>"

dia :: Diagram Canvas
dia = triangle 1 # fc blue <> square 1 # fc red
