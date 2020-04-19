{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeApplications          #-}


module Barrier.GitHub
  ( addStoryLinkComment
  , getCommentsForPullRequest
  , setHasStoryStatus
  , setMissingStoryStatus
  ) where

import           Barrier.Clubhouse.Types          (Story, storyUrl)
import           Barrier.Config                   (AppConfig, configGitHubToken)
import           Control.Logger.Simple            (logDebug)
import qualified Data.ByteString                  as B
import           Data.Generics.Product            (field)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Text.Read                   (decimal)
import           Data.Vector                      (Vector)
import           GHC.Generics                     (Generic)
import qualified GitHub.Auth                      as GitHub
import qualified GitHub.Data                      as GitHub
import           GitHub.Data.Webhooks.Payload
    ( HookPullRequest
    , getUrl
    , whPullReqHead
    , whPullReqIssueUrl
    , whPullReqTargetRepo
    , whPullReqTargetSha
    , whPullReqTargetUser
    , whRepoName
    , whUserLogin
    )
import qualified GitHub.Endpoints.Issues.Comments as GitHub
import qualified GitHub.Endpoints.Repos.Statuses  as GitHub
import           Lens.Micro.Platform              ((^.))
import           System.Random                    (randomRIO)
import           URI.ByteString
    (parseURI, serializeURIRef', strictURIParserOptions, uriPath)


data StatusParams = StatusParams
  { commit :: GitHub.Name GitHub.Commit
  , owner  :: GitHub.Name GitHub.Owner
  , repo   :: GitHub.Name GitHub.Repo
  } deriving (Generic, Show)


data GitHubRequestParams = GitHubRequestParams
  { commit :: GitHub.Name GitHub.Commit
  , owner  :: GitHub.Name GitHub.Owner
  , repo   :: GitHub.Name GitHub.Repo
  } deriving (Generic, Show)


--------------------------------------------------------------------------------
mkStatusParams :: HookPullRequest -> GitHubRequestParams
mkStatusParams pr =
  let head' = whPullReqHead pr
      _commit = GitHub.mkCommitName $ whPullReqTargetSha head'
      _owner = GitHub.mkOwnerName . whUserLogin $ whPullReqTargetUser head'
      _repo = GitHub.mkRepoName . whRepoName $ whPullReqTargetRepo head'
  in GitHubRequestParams {..}


--------------------------------------------------------------------------------
data GitHubCommentRequestParams = GitHubCommentRequestParams
  { commentIssue :: GitHub.IssueNumber
  , commentOwner :: GitHub.Name GitHub.Owner
  , commentRepo  :: GitHub.Name GitHub.Repo
  } deriving (Generic, Show)


--------------------------------------------------------------------------------
mkCommentParams :: HookPullRequest -> GitHubCommentRequestParams
mkCommentParams pr =
  let issueUrl = whPullReqIssueUrl pr
      head' = whPullReqHead pr
      _commentOwner = GitHub.mkOwnerName . whUserLogin $ whPullReqTargetUser head'
      _commentRepo = GitHub.mkRepoName . whRepoName $ whPullReqTargetRepo head'
      _commentIssue = getIssueId (encodeUtf8 $ getUrl issueUrl)
  in GitHubCommentRequestParams {..}


--------------------------------------------------------------------------------
setHasStoryStatus :: AppConfig -> HookPullRequest -> Story -> IO ()
setHasStoryStatus conf pr _story = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkStatusParams pr
  content <-
    either show show <$>
    GitHub.createStatus
      auth'
      (params ^. field @"owner")
      (params ^. field @"repo")
      (params ^. field @"commit")
      (GitHub.NewStatus
         GitHub.StatusSuccess
         Nothing
         (Just "Has link to clubhouse story.")
         (Just "Barrier story check"))
  logDebug $ T.pack (show content)


--------------------------------------------------------------------------------
setMissingStoryStatus :: AppConfig -> HookPullRequest -> IO ()
setMissingStoryStatus conf pr = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkStatusParams pr
  content <-
    either show show <$>
    GitHub.createStatus
      auth'
      (params ^. field @"owner")
      (params ^. field @"repo")
      (params ^. field @"commit")
      (GitHub.NewStatus
         GitHub.StatusError
         Nothing
         (Just "Cannot find matching story.")
         (Just "Barrier story check"))
  _ <- randomWarning conf pr
  logDebug $ T.pack (show content)


--------------------------------------------------------------------------------
statusesFor
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.Name GitHub.Commit
  -> IO (Either GitHub.Error (Vector GitHub.Status))
statusesFor auth' owner' repo' sha' =
  GitHub.statusesFor auth' owner' repo' sha'


-- | partial function that relies on github's promise to always return a valid issue url
getIssueId :: B.ByteString -> GitHub.IssueNumber
getIssueId url =
  let path = either (error . show) id $ uriPath <$> parseURI strictURIParserOptions url
      segments = B.split 47 path
      issueId = maybe (error "oh no") GitHub.IssueNumber (getId segments)
  in issueId
  where
    getId :: [B.ByteString] -> Maybe Int
    getId (_:_:_:_:_:x:_) = readish (decodeUtf8 x)
    getId _               = Nothing


--------------------------------------------------------------------------------
readish :: Integral a => Text -> Maybe a
readish s = either (const Nothing) (Just . fst) (decimal s)


--------------------------------------------------------------------------------
addStoryLinkComment :: AppConfig -> HookPullRequest -> Story -> IO ()
addStoryLinkComment conf pr story = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkCommentParams pr
  content <-
    either show show <$>
    GitHub.createComment
      auth'
      (params ^. field @"commentOwner")
      (params ^. field @"commentRepo")
      ((params ^. field @"commentIssue") :: GitHub.IssueNumber)
      (decodeUtf8 . serializeURIRef' $ storyUrl story)
  logDebug $ T.pack (show content)


--------------------------------------------------------------------------------
getCommentsForPullRequest :: AppConfig
                          -> HookPullRequest
                          -> IO (Either GitHub.Error (Vector GitHub.IssueComment))
getCommentsForPullRequest conf pr = do
  let auth' = Just . GitHub.OAuth $ configGitHubToken conf
  let params = mkCommentParams pr
  GitHub.comments'
    auth'
    (params ^. field @"commentOwner")
    (params ^. field @"commentRepo")
    (params ^. field @"commentIssue")


--------------------------------------------------------------------------------
randomWarning :: AppConfig -> HookPullRequest -> IO ()
randomWarning conf pr = do
  let auth' = GitHub.OAuth $ configGitHubToken conf
  let params = mkCommentParams pr
  value <- randomRIO (1, 7) :: IO Int
  if value == 3
    then filmReference auth' params
    else pure ()
  where
    filmReference auth' params = do
      _ <-
        either show show <$>
        GitHub.createComment
          auth'
          (params ^. field @"commentOwner")
          (params ^. field @"commentRepo")
          (params ^. field @"commentIssue")
          "Your move, creep"
      pure ()

-- updatePullRequest :: AppConfig -> HookPullRequest -> Story -> IO ()
-- updatePullRequest conf pr story = do
--   let auth' = GitHub.OAuth $ configGitHubToken conf
--   let params = mkCommentParams pr

--   -- issueId = maybe (error "oh no") (GitHub.mkId (Proxy :: Proxy GitHub.PullRequest)) (getId segments)
--   content <-
--     either show show <$>
--     GitHub.updatePullRequest
--       auth'
--       (params ^. commentOwner)
--       (params ^. commentRepo)
--       (params ^. commentThing)
--       undefined
--   logDebug $ T.pack (show content)
