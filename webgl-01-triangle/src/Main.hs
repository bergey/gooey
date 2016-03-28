{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

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

import Data.Bits
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
  inputs <- initShaderInputs context shaderProg
  buffers <- initBuffers context
  clearColor context 0 0 0 1
  enable context DEPTH_TEST
  drawScene context shaderProg inputs buffers
  return ()

data Inputs = Inputs
  { vertexPosition :: GLuint
  }

data BufferInfo = BufferInfo
  { buffer :: WebGLBuffer
  , itemSize :: GLint
  , attrType :: GLenum
                -- skip normalized, stride, offset
  , mode :: GLenum
  , firstI :: GLint
  , numItems :: GLsizei
  }

data Buffers = Buffers
  { triangles :: BufferInfo
  , squares :: BufferInfo
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
  Just canvas' <- getElementById doc name
  let canvas = coerce canvas'
  context <- coerce <$> getContext canvas "webgl"
  -- TODO error handling
  w <- getWidth canvas
  h <- getHeight canvas
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
  enableVertexAttribArray cxt $ fromIntegral pos
  return $ Inputs (fromIntegral pos)

foreign import javascript unsafe "new Float32Array([-0.5, 0.4, 0, -0.9, -0.4, 0, -0.1, -0.4, 0])" triangleArray :: JSVal

foreign import javascript unsafe "new Float32Array([0.9, 0.4, 0, 0.1, 0.4, 0, 0.9, -0.4, 0, 0.1, -0.4, 0])" squareArray :: JSVal

initBuffers :: WebGL2RenderingContext -> IO Buffers
initBuffers cxt = do
  Just triBuf <- createBuffer cxt
  bindBuffer cxt ARRAY_BUFFER (Just triBuf)
  bufferData cxt ARRAY_BUFFER  (Just $ ArrayBuffer triangleArray) STATIC_DRAW
  let tri = BufferInfo triBuf 3 FLOAT TRIANGLES 0 3
  Just sqBuf <- createBuffer cxt
  bindBuffer cxt ARRAY_BUFFER (Just sqBuf)
  bufferData cxt ARRAY_BUFFER (Just $ ArrayBuffer squareArray) STATIC_DRAW
  let sq = BufferInfo sqBuf 3 FLOAT TRIANGLE_STRIP 0 4
  return $ Buffers tri sq

-- TODO move attr location into BufferInfo
drawBuffer :: WebGL2RenderingContext ->  WebGLProgram -> GLuint -> BufferInfo -> IO ()
drawBuffer cxt prog attr BufferInfo{..} = do
  bindBuffer cxt ARRAY_BUFFER (Just buffer)
  vertexAttribPointer cxt attr itemSize attrType False 0 0
  drawArrays cxt mode firstI numItems

drawScene :: WebGL2RenderingContext -> WebGLProgram -> Inputs -> Buffers -> IO ()
drawScene cxt prog is bs = do
  clear cxt $ COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT
  drawBuffer cxt prog (vertexPosition is) (triangles bs)
  drawBuffer cxt prog (vertexPosition is) (squares bs)
