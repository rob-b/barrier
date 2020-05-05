{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barrier.Config
  ( AppConfig
  , configClubhouseToken
  , mkAppConfig
  , readish
  , configGitHubToken
  , configGitHubSecret
  , lookupEnv
  ) where

import           Data.ByteString    (ByteString)
import           Data.String        (IsString, fromString)
import           Data.Text          (Text)
import           Data.Text.Read     (decimal)
import qualified System.Environment as Env


data AppConfig = AppConfig
  { configGitHubToken    :: ByteString
  , configClubhouseToken :: ByteString
  , configGitHubSecret   :: ByteString
  , configEnvironment    :: Environment
  , configGitHubBot      :: ByteString
  } deriving (Show)


data Environment
  = Live
  | Development
  deriving (Show)


--------------------------------------------------------------------------------
lookupEnv
  :: IsString b
  => String -> IO (Maybe b)
lookupEnv envVar = do
  envMaybe <- Env.lookupEnv envVar
  pure $ fromString <$> envMaybe


--------------------------------------------------------------------------------
mkEnvironment
  :: (IsString a, Eq a)
  => a -> Maybe Environment
mkEnvironment s
  | s == "live" = Just Live
  | s == "development" = Just Development
  | otherwise = Nothing


--------------------------------------------------------------------------------
mkAppConfig :: IO (Maybe AppConfig)
mkAppConfig = do
  chTokenM <- lookupEnv "CLUBHOUSE_API_TOKEN"
  ghTokenM <- lookupEnv "GITHUB_API_TOKEN"
  ghSecretM <- lookupEnv "GITHUB_KEY"
  let ghBot = "robozd"
  (environmentNameM :: Maybe String) <- lookupEnv "BARRIER_ENV"
  let environment = mkEnvironment =<< environmentNameM
  let cfg = AppConfig <$> ghTokenM <*> chTokenM <*> ghSecretM <*> environment
  pure $ fmap (\cfg' -> cfg' ghBot) cfg


--------------------------------------------------------------------------------
readish :: Integral a => Text -> Maybe a
readish s = either (const Nothing) (Just . fst) (decimal s)
