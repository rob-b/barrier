
module Barrier.Events (noop)
   where
import           Control.Monad.IO.Class (MonadIO)


noop :: (MonadIO m) => a -> m ()
noop _ = pure ()
