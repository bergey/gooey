{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple timer using ghcjs-dom

module Main where

import           JsImports
import           Queue                   as Q

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement
-- import           GHCJS.DOM.Node
import           GHCJS.Foreign
-- import           GHCJS.Types

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad           hiding (sequence_)
import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import           Data.FileEmbed
import           Data.Foldable
import           Data.Text.Encoding
import           GHC.Exts                (IsString (..))

import           Prelude                 hiding (sequence_)

main :: IO ()
main = do
  -- initialize mutable variables
  eventQueue <- newQueue :: IO (Queue Event)
  t0 <- now -- ms
  state <- newMVar $ AppState 120 t0 t0
  -- render the static parts of the HTML
  -- TODO insert the CSS, also
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body $ decodeUtf8 initialHtml
  -- bind callbacks that fire events
  traverse_ (uncurry $ onclick doc eventQueue) buttons
  -- render in a loop
  animate =<< syncCallback NeverRetain False (render state)
  -- fork a thread for the event handler
  _ <- forkIO $ queueHandler state eventQueue handleEvent
  -- register a timer for the countdown, interval in ms
  -- setInterval 500 (timer eventQueue)
  -- modifyMVar_ state $ \(AppState s t0 t) -> return (AppState (s+1) t0 t)
  forkIO $ timer eventQueue
    -- modifyMVar_ state $ \(AppState s t0 t) -> return (AppState (s-1) t0 t)
    -- threadDelay 1000000
  return ()

data AppState = AppState
                Int -- ^ Seconds at which timer started
                Double -- ^ ms since epoch when timer started
                Double -- ^ last tick
              deriving Show

data Event
  = Day Step
  | Hour Step
  | Minute Step
  | Second Step
  | Tick Double
  deriving Show

data Step = Up | Down
          deriving Show

render :: MVar AppState -> IO ()
render sVar = do
  Just doc <- currentDocument
  AppState s0 t0 t <- readMVar sVar
  let ids = ["dday", "dhour", "dmin", "dsec"]
      s = s0 - floor ((t - t0) / 1e3)
      nums = map show
             [ s `div` sPerDay
             , mod s sPerDay `div` sPerHour
             , mod s sPerHour `div` sPerMinute
             , s `mod` sPerMinute
             ]
  sequence_ $ zipWith (setText doc) ids nums

setText :: Document -> String -> String -> IO ()
setText doc elemId val = do
  Just elem <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc elemId
  htmlElementSetInnerText elem val

onclick :: Document -> Queue Event -> String -> Event -> IO ()
onclick doc q elemId ev = do
  mayElem <- (fmap . fmap) castToHTMLElement $ documentGetElementById doc elemId
  case mayElem of
    Nothing -> putStrLn $ "error: could not find element with id: " ++ elemId
    Just elem -> do
      elementOnclick elem . liftIO $ push q ev
      return ()

-- | Each pair is an element ID and the Event raised by the element.
buttons :: [(String, Event)]
buttons =
  [ ("plus-day", Day Up)
  , ("plus-hour", Hour Up)
  , ("plus-minute", Minute Up)
  , ("plus-second", Second Up)
  , ("minus-day", Day Down)
  , ("minus-hour", Hour Down)
  , ("minus-minute", Minute Down)
  , ("minus-second", Second Down)
  ]

-- | The specific event handler for this application
handleEvent :: Event -> AppState -> IO AppState
handleEvent ev (AppState s t0 t) = return $ AppState (f ev) t0 tick where
  f (Day step) = g step sPerDay
  f (Hour step) = g step sPerHour
  f (Minute step) = g step sPerMinute
  f (Second step) = g step 1
  f _ = s
  g Up ss = s + ss
  g Down ss = max 0 (s - ss)
  tick = case ev of
    Tick t' -> t'
    _ -> t

timer :: Queue Event -> IO ()
timer q = forever $ do
  t <- now
  push q $ Tick t
  threadDelay 500000

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Queue e -> (e -> s -> IO s) -> IO ()
queueHandler s q h = pop q >>= \case
  -- With only one thread reading the queue, Nothing should never
  -- occur.  But this is an OK way to handle it anyway.
  -- 1e4 µs = 0.01 seconds
  Nothing -> threadDelay 10000 >> queueHandler s q h
  Just ev -> modifyMVar_ s (h ev) >> queueHandler s q h

setInterval :: Double -> IO () -> IO ()
setInterval t act = windowSetInterval t =<< syncCallback AlwaysRetain True act

sPerMinute, sPerHour, sPerDay :: Int
sPerMinute = 60
sPerHour = 60 * sPerMinute
sPerDay = 24 * sPerHour

initialHtml :: ByteString
initialHtml = $(embedFile "timer/inner.html")
