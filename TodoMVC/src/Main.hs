module Main where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad           (forever, void)

main :: IO ()
main = do
  q <- newChan
  -- loop, push to Chan
  void . forkIO . forever $ do
    writeChan q ()
    threadDelay 1000000
  -- read, read from Chan
  void . forever $ readChan q
