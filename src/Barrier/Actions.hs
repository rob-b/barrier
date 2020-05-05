{-# LANGUAGE OverloadedStrings #-}

module Barrier.Actions where

import           Barrier.Clubhouse       (getStory)
import           Barrier.Clubhouse.Types (ClubhouseLink, Story, StoryError)
import           Barrier.Config          (AppConfig)
import           Control.Error           (ExceptT)
import           Control.Logger.Simple   (logDebug)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (runReaderT)
import           Data.Either             (lefts)
import qualified Data.Text               as T


--------------------------------------------------------------------------------
linkDebug :: (Show a, MonadIO m) => a -> m ()
linkDebug link = logDebug ("Checking link: " <> T.pack (show link))


--------------------------------------------------------------------------------
getStoryForLink :: (MonadIO m) => AppConfig -> ClubhouseLink -> m (ExceptT StoryError IO Story)
getStoryForLink config l = linkDebug l >> runReaderT (getStory l) config


--------------------------------------------------------------------------------
sequenceEithers :: [Either a b] -> Either [a] [b]
sequenceEithers xs =
    case sequence xs of
      Right values -> Right values
      Left _       -> Left (lefts xs)
