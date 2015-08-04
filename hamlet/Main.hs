{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A simple example using getElementById and file-embed

module Main where

import GHCJS.DOM.HTMLElement
import GHCJS.DOM
import GHCJS.DOM.Document
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body $ renderHtml initialHtml
  Just days <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc "dday"
  htmlElementSetInnerText days "1"
  Just hours <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc "dhour"
  htmlElementSetInnerText hours "2"
  Just minutes <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc "dmin"
  htmlElementSetInnerText minutes "3"
  Just seconds <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc "dsec"
  htmlElementSetInnerText seconds "4"


initialHtml :: Html
initialHtml = [shamlet| $newline always
<table>
  <tr>
    <td>+</td>
    <td>+</td
    <td>+</td
    <td>+</td
  <tr>
    <td #dday>
    <td #dhour>
    <td #dmin>
    <td #dsec>
  <tr>
    <td>Days</td>
    <td>Hours></td>
    <td>Minutes</td>
    <td>Seconds</td>
  <tr>
    <td>-</td>
    <td>-</td
    <td>-</td
    <td>-</td
|]
