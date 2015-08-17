{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple timer using ghcjs-dom

module Main where

import           JsImports

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Types               hiding (Event)
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Web.AnimationFrame

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Control.Monad.IO.Class
import           Data.ByteString               (ByteString)
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           GHC.Exts                      (IsString (..))

import           Prelude                       hiding (sequence_)

main :: IO ()
main = do
  -- initialize mutable variables
  eventQueue <- newChan :: IO (Chan Event)
  t0 <- now -- ms
  state <- newMVar $ AppState 120 t0 t0 True
  -- render the static parts of the HTML
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just . T.unpack . decodeUtf8 $ initialHtml
  -- bind callbacks that fire events
  traverse_ (uncurry $ onclick doc eventQueue) buttons
  -- render in a loop
  animate $ render state
  -- fork a thread for the event handler
  _ <- forkIO $ queueHandler state eventQueue handleEvent
  -- register a timer for the countdown, interval in ms
  void $ setInterval (timer eventQueue) 500

data AppState = AppState
                Int -- ^ Seconds at which timer started
                Double -- ^ ms since epoch when timer started
                Double -- ^ last tick
                Bool -- ^ Running?
              deriving Show

data Event
  = Day Step
  | Hour Step
  | Minute Step
  | Second Step
  | Tick Double
  | Pause
  | Resume
  | Stop -- ^ stop, forgetting how long timer ran
  deriving Show

data Step = Up | Down
          deriving Show

render :: MVar AppState -> IO ()
render sVar = do
  Just doc <- currentDocument
  AppState s0 t0 t _ <- readMVar sVar
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
  Just elem <- (fmap . fmap) castToHTMLElement $ getElementById doc elemId
  setInnerText elem $ Just val

onclick :: Document -> Chan Event -> String -> Event -> IO ()
onclick doc q elemId ev = do
  mayElem <- getElementById doc elemId
  case mayElem of
    Nothing -> putStrLn $ "error: could not find element with id: " ++ elemId
    Just elem -> do
      cb <- eventListenerNew (const $ writeChan q ev :: MouseEvent -> IO ())
      addEventListener elem "click" (Just cb) False

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
  , ("pause", Pause)
  , ("resume", Resume)
  , ("stop", Stop)
  ]

-- | The specific event handler for this application
handleEvent :: Event -> AppState -> IO AppState
handleEvent (Tick t) (AppState s t0 _ True) =
  if s - (floor $ (t - t0) / 1e3) > 0
  then return $ AppState s t0 t True
  else return $ AppState s t t False
handleEvent (Tick t) st = return st
handleEvent Pause (AppState s t0 t _) = return $ AppState s t0 t False
handleEvent Resume (AppState s t0 t1 False) = do
  t <- now
  return $ AppState s (t - (t1 - t0)) t True
handleEvent Stop (AppState s _ _ _) = do
  t <- now
  return $ AppState s t t False
-- plus & minus buttons
handleEvent ev (AppState s t0 t r) = return $ AppState (f ev) t0 t r where
  f (Day step) = g step sPerDay
  f (Hour step) = g step sPerHour
  f (Minute step) = g step sPerMinute
  f (Second step) = g step 1
  f _ = s
  g Up ss = s + ss
  g Down ss = max 0 (s - ss)

timer :: Chan Event -> IO ()
timer q = do
  t <- now
  writeChan q $ Tick t

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Chan e -> (e -> s -> IO s) -> IO ()
queueHandler s q h = do
  ev <- readChan q
  modifyMVar_ s (h ev)
  queueHandler s q h

setInterval :: IO () -> Int -> IO Int
setInterval act t = do
  cb <- toJSRef =<< syncCallback ContinueAsync act
  js_setInterval cb t

sPerMinute, sPerHour, sPerDay :: Int
sPerMinute = 60
sPerHour = 60 * sPerMinute
sPerDay = 24 * sPerHour

initialHtml :: ByteString
initialHtml = $(embedFile "timer/inner.html")

-- | render in a loop, using @requestAnimationFrame@
animate :: IO () -> IO ()
animate act = void $ inAnimationFrame ContinueAsync (act >> animate act)
