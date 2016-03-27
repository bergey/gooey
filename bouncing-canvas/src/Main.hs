{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           JsImports                     (now)
import qualified JavaScript.Web.Canvas          as C

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Types                hiding (Event)
import           GHCJS.DOM.UIEvent

import qualified JavaScript.Web.Canvas.Internal as C

import           GHCJS.Foreign
import           GHCJS.Types

import           Linear

import           Data.Coerce
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Foldable                 (minimumBy)
import           Data.Ord
import           Data.Semigroup

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
  Just body <- getBody doc
  setInnerHTML body $ Just initialHtml
  Just canvas <- getElementById doc "dia"
  ctx <- getContext canvas
  cb <- eventListenerNew $ newBall state canvas
  addEventListener canvas "click" (Just cb) False
  forever $ do
    s <- takeMVar state
    render ctx s
    s' <- physics s
    putMVar state s'
    threadDelay 10000

render :: C.Context -> State -> IO ()
render ctx s@(State{pos}) = do
  C.clearRect 0 0 200 200 ctx
  C.beginPath ctx
  circle pos
  C.fill ctx
    where
      circle (V2 x y) = C.arc (200*x) (200*y) 2 0 (2*pi) False ctx

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
    x <- fromIntegral <$> getPageX ev
    y <- fromIntegral <$> getPageY ev
    x0 <- getOffsetLeft canvas
    y0 <-  getOffsetTop canvas
    let x' = (x - x0) / 200
    let y' = (y - y0) / 200
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
  C.getContext . coerce $ el

initialHtml :: String
initialHtml = "<canvas id=\"dia\" width=\"200\" height=\"200\" style=\"border: 1px solid\"></canvas>"
