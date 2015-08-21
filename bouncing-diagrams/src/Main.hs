{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           JsImports               (now)

import           Diagrams.Backend.GHCJS
import           Diagrams.Prelude        hiding (Time, render)
import qualified JavaScript.Canvas       as C
import           JavaScript.JQuery       (append, select)

import           GHCJS.Foreign
import           GHCJS.Types

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad           hiding (sequence_)
import           Data.Foldable           (minimumBy)
import           Data.Ord
import qualified Data.Text               as T

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
  t0 <- now
  state <- newMVar $ initialState t0
  body <- select "body"
  append initialHtml body
  ctx <- getContextById "dia"
  forever $ do
    s <- takeMVar state
    render ctx s
    s' <- physics s
    putMVar state s'
    threadDelay 100000

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
  print $ t - time s
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

getContextById :: T.Text -> IO C.Context
getContextById name =
  C.getContext =<< indexArray 0 . castRef =<< select name' where
    name' = "#" <> name

initialHtml :: T.Text
initialHtml = "<canvas id=\"dia\" width=\"200\" height=\"200\" style=\"border: 1px solid\"></canvas>"
