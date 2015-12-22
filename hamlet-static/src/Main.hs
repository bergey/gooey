{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple example using getElementById and file-embed

module Main where

import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamlet)

import           Data.JSString.Text
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just . lazyTextToJSString $ renderHtml initialHtml
  Just days <- getElementById doc "dday"
  setInnerHTML days $ Just "1"
  Just hours <- getElementById doc "dhour"
  setInnerHTML hours $ Just "2"
  Just minutes <- getElementById doc "dmin"
  setInnerHTML minutes $ Just "3"
  Just seconds <- getElementById doc "dsec"
  setInnerHTML seconds $ Just "4"


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
