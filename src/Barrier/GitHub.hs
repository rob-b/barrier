{-# LANGUAGE OverloadedStrings #-}
module Barrier.GitHub where


import           Data.Monoid                     ((<>))
import           Data.String                     (IsString, fromString)
import           Data.Vector                     (Vector)
import qualified GitHub.Auth                     as GitHub
import qualified GitHub.Data                     as GitHub
import           GitHub.Data.Webhooks.Payload    (HookPullRequest, whPullReqHead,
                                                  whPullReqTargetRepo, whPullReqTargetSha,
                                                  whPullReqTargetUser, whRepoName, whUserLogin)
import qualified GitHub.Endpoints.Repos.Statuses as GitHub
import qualified System.Environment              as Env


setMissingStoryStatus :: HookPullRequest -> IO ()
setMissingStoryStatus pr = do
  let head' = whPullReqHead pr
  let sha = GitHub.mkCommitName $ whPullReqTargetSha head'
  let owner = GitHub.mkOwnerName . whUserLogin $ whPullReqTargetUser head'
  let repo = GitHub.mkRepoName . whRepoName $ whPullReqTargetRepo head'

  auth <- mkAuth
  content <- case auth of
    Nothing    -> pure "Missing auth token"
    Just auth' -> either show show <$> newStatus auth' owner repo sha
  appendFile "example.txt" (show content <> "\n")


lookupEnv :: IsString b => String -> IO (Maybe b)
lookupEnv envVar = do
    envMaybe <- Env.lookupEnv envVar
    pure $ fromString <$> envMaybe


mkAuth :: IO (Maybe GitHub.Auth)
mkAuth = lookupEnv "GITHUB_API_TOKEN" >>= \mt -> pure $ GitHub.OAuth <$> mt


newStatus
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.Name GitHub.Commit
  -> IO (Either GitHub.Error GitHub.Status)
newStatus auth' owner repo sha = do
  GitHub.createStatus
    auth'
    owner
    repo
    sha
    (GitHub.NewStatus
       GitHub.StatusError
       Nothing
       (Just "Cannot find matching story.")
       (Just "Barrier check (is there a corresponding story)"))


statusesFor
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.Name GitHub.Commit
  -> IO (Either GitHub.Error (Vector GitHub.Status))
statusesFor auth' owner repo sha = do
  GitHub.statusesFor auth' owner repo sha
