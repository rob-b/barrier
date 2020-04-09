{-# LANGUAGE OverloadedStrings #-}

module Barrier.Events (noop)
   where
import           Control.Logger.Simple  (logInfo)
import           Control.Monad.IO.Class (MonadIO)


noop :: (MonadIO m) => a -> m ()
noop _ = logInfo "I am a noop action that does nothing"
