{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           JsImports                     (now)

import           Diagrams.Backend.GHCJS
import           Diagrams.Prelude              hiding (Time, render)
import           GHCJS.DOM                     (currentDocument, currentWindow)
import           GHCJS.DOM.Document            (documentGetBody,
                                                documentGetElementById)
import           GHCJS.DOM.Element             (elementGetOffsetLeft,
                                                elementGetOffsetTop)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLElement         (htmlElementGetInnerHTML,
                                                htmlElementSetInnerHTML)
import           GHCJS.DOM.Types               (Element, IsDocument, MouseEvent,
                                                unElement)
import           GHCJS.DOM.UIEvent             (uiEventGetPageX,
                                                uiEventGetPageY)
import qualified JavaScript.Canvas             as C

import           GHCJS.Foreign
import           GHCJS.Types

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Foldable                 (minimumBy)
import           Data.Ord

data State = State
               { pos      :: V2D
               , velocity :: V2D -- ^ per millisecond
               , time     :: Time
               } deriving Show

initialState t = State (V2 0.5 0.5) (V2 3e-4 2e-4) t

type Time = Double -- ^ milliseconds
type TimeDelta = Double
type V2D = V2 Double

main :: IO ()
main = do
  -- initialize MVar
  t0 <- now
  state <- newMVar $ initialState t0
  -- get Canvas context
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body initialHtml
  Just canvas <- documentGetElementById doc "dia"
  ctx <- getContext canvas
  eventTargetAddEventListener canvas "click" False $ newBall state
  forever $ do
    s <- takeMVar state
    render ctx s
    s' <- physics s
    putMVar state s'
    threadDelay 10000

render :: C.Context -> State -> IO ()
render ctx s@(State{pos}) = do
  C.clearRect 0 0 200 200 ctx
  renderDia Canvas (CanvasOptions (dims2D 200 200) ctx) dia
    where
      dia :: Diagram Canvas
      dia = circle 0.01 # translate pos # fc blue # clipped (square 1 # translate (V2 0.5 0.5))

physics :: State -> IO State
physics s = do
  t <- now
  -- print $ t - time s
  return $ physics' s t

physics' :: State -> Time -> State
physics' s@(State { pos, velocity, time}) t = case collision s t of
  [] -> State pos1 velocity t
    where pos1 = pos + velocity ^* (t - time)
  cs@((_,tCol):_) -> State pos2 v' t
    where
      -- tCol is Δt from last step to collision
      v' = foldr reflect velocity $ map fst cs
      pos2 = pos + tCol *^ velocity + (t - time - tCol) *^ v'

newBall :: MVar State -> Element -> MouseEvent -> IO ()
newBall mstate canvas ev = do
    Just win <- currentWindow
    x <- fromIntegral <$> uiEventGetPageX ev
    y <- fromIntegral <$> uiEventGetPageY ev
    x0 <- elementGetOffsetLeft canvas
    y0 <-  elementGetOffsetTop canvas
    let x' = (x - x0) / 200
    let y' = 1 - (y - y0) / 200
    modifyMVar_ mstate (\s -> return $ s {pos = V2 x' y'})

data Edge = East | West | North | South

-- | It is possible to hit 0, 1, or 2 Edges
collision :: State -> Time -> [(Edge, TimeDelta)]
collision s@State{pos, velocity, time} t =
  case filter ((>= 0) . snd) . filter ((< dt) . snd) . map (intersect s) $ edges of
      [] -> []
      ts -> filter ((==tMin) . snd) ts where
             tMin = minimum $ map snd ts
  where
    dt = t - time
    edges = [ East, West, North, South ]

nextPosition :: State -> Time -> V2D
nextPosition State{pos, velocity, time} t =
  (t - time) *^ velocity ^+^ pos

intersect :: State -> Edge -> (Edge, TimeDelta)
intersect s e = (e, t) where
  t = case e of
    East -> (1 - x) / vx
    West -> -x / vx
    North -> (1 - y) / vy
    South -> -y / vy
  (V2 x y) = pos s
  (V2 vx vy) = velocity s

reflect :: Edge -> V2D -> V2D
reflect East (V2 x y) = V2 (-x) y
reflect West (V2 x y) = V2 (-x) y
reflect North (V2 x y) = V2 x (-y)
reflect South (V2 x y) = V2 x (-y)

getContext :: Element -> IO C.Context
getContext el = do
  C.getContext . castRef . unElement $ el

initialHtml :: String
initialHtml = "<canvas id=\"dia\" width=\"200\" height=\"200\" style=\"border: 1px solid\"></canvas>"
