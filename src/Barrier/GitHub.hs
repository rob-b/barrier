{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Barrier.GitHub where


import           Barrier.Clubhouse               (Story)
import           Barrier.Config                  (AppConfig, configGitHubToken)
import           Control.Logger.Simple           (logDebug)
import qualified Data.Text                       as T
import           Data.Vector                     (Vector)
import qualified GitHub.Auth                     as GitHub
import qualified GitHub.Data                     as GitHub
import           GitHub.Data.Webhooks.Payload    (HookPullRequest, whPullReqHead,
                                                  whPullReqTargetRepo, whPullReqTargetSha,
                                                  whPullReqTargetUser, whRepoName, whUserLogin)
import qualified GitHub.Endpoints.Repos.Statuses as GitHub
import           Lens.Micro.Platform             (makeLenses, (^.))


data GitHubRequestParams = GitHubRequestParams
  { _commit :: GitHub.Name GitHub.Commit
  , _owner  :: GitHub.Name GitHub.Owner
  , _repo   :: GitHub.Name GitHub.Repo
  } deriving (Show)


makeLenses ''GitHubRequestParams


mkStatusParams :: HookPullRequest -> GitHubRequestParams
mkStatusParams pr =
  let head' = whPullReqHead pr
      _commit = GitHub.mkCommitName $ whPullReqTargetSha head'
      _owner = GitHub.mkOwnerName . whUserLogin $ whPullReqTargetUser head'
      _repo = GitHub.mkRepoName . whRepoName $ whPullReqTargetRepo head'
  in GitHubRequestParams {..}


setHasStoryStatus :: AppConfig -> HookPullRequest -> Story -> IO ()
setHasStoryStatus conf pr _story = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkStatusParams pr
  content <-
    either show show <$>
    GitHub.createStatus
      auth'
      (params ^. owner)
      (params ^. repo)
      (params ^. commit)
      (GitHub.NewStatus
         GitHub.StatusSuccess
         Nothing
         (Just "Has link to clubhouse story.")
         (Just "Barrier story check"))
  logDebug $ T.pack (show content)


setMissingStoryStatus :: AppConfig -> HookPullRequest -> IO ()
setMissingStoryStatus conf pr = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkStatusParams pr
  content <-
    either show show <$>
    GitHub.createStatus
      auth'
      (params ^. owner)
      (params ^. repo)
      (params ^. commit)
      (GitHub.NewStatus
         GitHub.StatusError
         Nothing
         (Just "Cannot find matching story.")
         (Just "Barrier story check"))
  logDebug $ T.pack (show content)


statusesFor
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.Name GitHub.Commit
  -> IO (Either GitHub.Error (Vector GitHub.Status))
statusesFor auth' owner' repo' sha' =
  GitHub.statusesFor auth' owner' repo' sha'
