module Barrier.Config where

import           Data.ByteString    (ByteString)
import           Data.String        (IsString, fromString)
import qualified System.Environment as Env


data AppConfig = AppConfig
  { configGitHubToken    :: ByteString
  , configClubhouseToken :: ByteString
  , configGitHubSecret   :: ByteString
  } deriving (Show)


lookupEnv
  :: IsString b
  => String -> IO (Maybe b)
lookupEnv envVar = do
  envMaybe <- Env.lookupEnv envVar
  pure $ fromString <$> envMaybe


mkAppConfig :: IO (Maybe AppConfig)
mkAppConfig = do
  chTokenM <- lookupEnv "CLUBHOUSE_API_TOKEN"
  ghTokenM <- lookupEnv "GITHUB_API_TOKEN"
  ghSecretM <- lookupEnv "GITHUB_KEY"
  pure $ fmap AppConfig ghTokenM <*> chTokenM <*> ghSecretM
