module Barrier.Queue
  ( add
  , make
  , worker
  , Action
  ) where


import           Control.Concurrent.STM.TBMQueue
    (TBMQueue, newTBMQueueIO, readTBMQueue, tryWriteTBMQueue)
import           Control.Monad                   (unless)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.STM               (atomically)


type Action = IO ()


--------------------------------------------------------------------------------
make :: (MonadIO m) => Int -> m (TBMQueue Action)
make = liftIO . newTBMQueueIO


--------------------------------------------------------------------------------
add :: TBMQueue Action -> Action -> IO ()
add q action = do
  addM <- atomically $ tryWriteTBMQueue q action
  case addM of
    Nothing    -> putStrLn "Is the queue closed"
    Just added -> unless added $ putStrLn "Failed to add action"


--------------------------------------------------------------------------------
worker :: TBMQueue Action -> IO ()
worker q = do
  let go = do
        actionM <- atomically $ readTBMQueue q
        case actionM of
          Nothing -> pure ()
          Just action -> do
            _ <- action
            go
  go
