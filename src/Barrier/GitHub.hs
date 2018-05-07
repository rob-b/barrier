{-# LANGUAGE OverloadedStrings #-}
module Barrier.GitHub where


import           Barrier.Config                  (AppConfig, configGitHubToken)
import           Data.Monoid                     ((<>))
import           Data.Vector                     (Vector)
import qualified GitHub.Auth                     as GitHub
import qualified GitHub.Data                     as GitHub
import           GitHub.Data.Webhooks.Payload    (HookPullRequest, whPullReqHead,
                                                  whPullReqTargetRepo, whPullReqTargetSha,
                                                  whPullReqTargetUser, whRepoName, whUserLogin)
import qualified GitHub.Endpoints.Repos.Statuses as GitHub


setMissingStoryStatus :: AppConfig -> HookPullRequest -> IO ()
setMissingStoryStatus conf pr = do
  let head' = whPullReqHead pr
  let sha = GitHub.mkCommitName $ whPullReqTargetSha head'
  let owner = GitHub.mkOwnerName . whUserLogin $ whPullReqTargetUser head'
  let repo = GitHub.mkRepoName . whRepoName $ whPullReqTargetRepo head'
  let auth' = GitHub.OAuth $ configGitHubToken conf
  content <- either show show <$> newStatus auth' owner repo sha
  appendFile "example.txt" (show content <> "\n")


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
