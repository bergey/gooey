{-# LANGUAGE TemplateHaskell #-}

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Types                hiding (Event)
import           GHCJS.DOM.UIEvent
import           GHCJS.DOM.WebGLRenderingContextBase
import           GHCJS.DOM.HTMLCanvasElement

import qualified JavaScript.Web.Canvas.Internal as C

import           GHCJS.Foreign
import           GHCJS.Types

-- import           Linear

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import           Data.FileEmbed (embedFile)
import           Data.Coerce
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Foldable                 (minimumBy)
import           Data.Ord
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
  initHTML
  context <- initGL "webgl0"
  Just shaderProg <- initShaders context
  initShaderInputs context shaderProg
  -- buffers <- initBuffers context
  -- clearColor context 0 0 0 1
  -- enable context DEPTH_TEST
  -- drawScene context shaderProg buffers
  return ()

data Inputs = Inputs
  { vertexPosition :: GLint
  , uPMatrix :: WebGLUniformLocation
  , uMVMatrix :: WebGLUniformLocation
  }

initHTML :: IO ()
initHTML = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just $
    "<canvas id=\"webgl0\" width=\"200\" height=\"200\" style=\"border: 1px solid\"></canvas>"

initGL :: String -> IO WebGL2RenderingContext
initGL name = do
  Just doc <- currentDocument
  Just canvas <- coerce <$> getElementById doc name
  context <- coerce <$> getContext canvas "webgl"
  -- TODO error handling
  w <- getWidth canvas
  h <- getWidth canvas
  viewport context 0 0 (fromIntegral w) (fromIntegral h)
  return context

initShaders :: IsWebGLRenderingContextBase self
               => self -> IO (Maybe WebGLProgram)
initShaders context = do
  fragmentShader <- createShader context FRAGMENT_SHADER
  shaderSource context fragmentShader fragmentShaderSource
  compileShader context fragmentShader
  -- TODO better error handling

  vertexShader <- createShader context VERTEX_SHADER
  shaderSource context vertexShader vertexShaderSource
  compileShader context vertexShader

  shaderProgram <- createProgram context
  attachShader context shaderProgram fragmentShader
  attachShader context shaderProgram vertexShader
  linkProgram context shaderProgram
  return shaderProgram

vertexShaderSource :: String
vertexShaderSource = unpack $(embedFile "src/triangle.vert")

fragmentShaderSource :: String
fragmentShaderSource = unpack $(embedFile "src/triangle.frag")

initShaderInputs :: WebGL2RenderingContext -> WebGLProgram -> IO Inputs
initShaderInputs cxt shaderProgram = do
  let shaderProgram' = Just shaderProgram
  useProgram cxt $ Just shaderProgram
  pos <- getAttribLocation cxt shaderProgram' "aVertexPosition"
  Just uPM <- getUniformLocation cxt shaderProgram' "uPMatrix"
  Just uMVM <- getUniformLocation cxt shaderProgram' "uMVMatrix"
  return $ Inputs pos uPM uMVM

-- initBuffers ::
