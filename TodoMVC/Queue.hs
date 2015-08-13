{-# LANGUAGE LambdaCase #-}

-- | An MVar FIFO Queue
-- based on http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html

module Queue where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Sequence           as S

type Queue a = MVar (Either  (Seq a) (Barrier a))

-- | A barrier starts with no value, is written to once, and read one or more times.
type Barrier a = MVar a

newQueue :: IO (Queue a)
newQueue = newMVar $ Left S.empty

push :: Queue a -> a -> IO ()
push q x = modifyMVar_ q $ \case
  Left s -> return . Left $ x <| s
  Right b -> putMVar b x >> return (Left S.empty)


-- | @pop@ returns the value that has been in the 'Queue' longest.  The
-- first thread calling @pop@ blocks until an element is pushed.  Any
-- calls to @pop@ while another thread is waiting return 'Nothing'.
pop :: Queue a -> IO (Maybe a)
pop q = join $ modifyMVar q $ \case
  Left s -> case viewr s of
    s' :> r -> return (Left s', return $ Just r)
    EmptyR -> do
      b <- newEmptyMVar
      return (Right b, Just <$> readMVar b)
  Right b -> return (Right b, return Nothing)

-- | For a non-empty sequence, returns the right-most element and the
-- sequence remaining when this element is removed.  For an empty
-- Sequence, returns the unchanged Sequence and Nothing.
safeTailHeadR :: Seq a -> (Seq a, Maybe a)
safeTailHeadR s = case viewr s of
  EmptyR -> (s, Nothing)
  s' :> r -> (s', Just r)

-- | A helper function to hook an event handler to an event queue
queueHandler :: MVar s -> Queue e -> (e -> s -> IO s) -> IO ()
queueHandler s q h = pop q >>= \case
  -- With only one thread reading the queue, Nothing should never
  -- occur.  But this is an OK way to handle it anyway.
  -- 1e4 µs = 0.01 seconds
  Nothing -> threadDelay 10000 >> queueHandler s q h
  Just ev -> modifyMVar_ s (h ev) >> queueHandler s q h
