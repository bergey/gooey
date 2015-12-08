{-# LANGUAGE TemplateHaskell #-}

-- | A simple example using getElementById and file-embed

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.FileEmbed
import           Data.Text.Encoding

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just $ decodeUtf8 initialHtml
  Just days <- (fmap . fmap) castToHTMLElement $ getElementById doc "dday"
  setInnerText days $ Just "1"
  Just hours <- (fmap . fmap) castToHTMLElement $ getElementById doc "dhour"
  setInnerText hours $ Just "2"
  Just minutes <- (fmap . fmap) castToHTMLElement $ getElementById doc "dmin"
  setInnerText minutes $ Just "3"
  Just seconds <- (fmap . fmap) castToHTMLElement $ getElementById doc "dsec"
  setInnerText seconds $ Just "4"


initialHtml :: ByteString
initialHtml = $(embedFile "inner.html")
