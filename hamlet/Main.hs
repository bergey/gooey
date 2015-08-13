{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple example using getElementById and file-embed

module Main where

import qualified Data.Text.Lazy                as TL
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement
import           GHCJS.Foreign
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamlet)

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just . TL.unpack . renderHtml $ initialHtml
  Just days <- (fmap . fmap) castToHTMLElement $ getElementById doc "dday"
  setInnerText days $ Just "1"
  Just hours <- (fmap . fmap) castToHTMLElement $ getElementById doc "dhour"
  setInnerText hours $ Just "2"
  Just minutes <- (fmap . fmap) castToHTMLElement $ getElementById doc "dmin"
  setInnerText minutes $ Just "3"
  Just seconds <- (fmap . fmap) castToHTMLElement $ getElementById doc "dsec"
  setInnerText seconds $ Just "4"


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
