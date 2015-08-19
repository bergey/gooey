{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Diagrams.Backend.GHCJS
import           Diagrams.Prelude
import qualified JavaScript.Canvas      as C
import           JavaScript.JQuery      (append, select)

import           GHCJS.Foreign
import           GHCJS.Types

import qualified Data.Text              as T

main :: IO ()
main = do
  body <- select "body"
  append initialHtml body
  ctx <- getContextById "dia"
  renderDia Canvas (CanvasOptions (Dims 200 200) ctx) dia

getContextById :: T.Text -> IO C.Context
getContextById name =
  C.getContext =<< indexArray 0 . castRef =<< select name' where
    name' = "#" <> name

initialHtml :: T.Text
initialHtml = "<canvas id=\"dia\" width=\"200\" height=\"200\"></canvas>"

dia :: Diagram Canvas R2
dia = triangle 1 # fc blue <> square 1 # fc red
