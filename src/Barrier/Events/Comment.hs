{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.Comment
  ( handleCommentEvent
  , handleIssueCommentEventAction
  , isSupportedEvent
  , doThingForComment
  , pullRequestForIssueCommentEvent
  ) where


import           Barrier.Actions               (getStoryForLink)
import           Barrier.Check                 (extractClubhouseLinks2)
import           Barrier.Clubhouse.Types       (ClubhouseLink, Story(Story), StoryError)
import           Barrier.Config                (AppConfig, configGitHubToken)
import           Barrier.Events                (noop)
import           Barrier.GitHub
    (GitHubRequestParams(GitHubRequestParams), commit, owner, repo, setHasStoryStatus')
import           Control.Error                 (ExceptT, runExceptT)
import           Control.Logger.Simple         (logDebug)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (Value, object, (.=))
import qualified Data.ByteString.Char8         as C
import           Data.Either                   (partitionEithers)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Vector                   as V
import           Debug.Trace                   (traceShow)
import qualified GitHub.Data                   as GitHub
import           GitHub.Data.Id                (Id(Id))
import           GitHub.Data.PullRequests      (pullRequestHead)
import           GitHub.Data.Webhooks.Events
    ( IssueCommentEvent
    , IssueCommentEventAction(IssueCommentCreatedAction, IssueCommentEditedAction)
    , evIssueCommentAction
    , evIssueCommentIssue
    , evIssueCommentPayload
    , evIssueCommentRepo
    )
import           GitHub.Data.Webhooks.Payload
    ( URL
    , getUrl
    , whIssueCommentBody
    , whIssueCommentHtmlUrl
    , whIssueCommentUser
    , whIssueNumber
    , whRepoName
    , whRepoOwner
    , whSimplUserName
    , whUserLogin
    )
import           GitHub.Endpoints.PullRequests (pullRequest')
import           URI.ByteString                (parseURI, strictURIParserOptions, uriPath)


handleCommentEvent :: IssueCommentEvent -> Value
handleCommentEvent event =
  let inner   = V.singleton $ object ["comment" .= comment]
      comment = whIssueCommentBody $ evIssueCommentPayload event
  in  object ["data" .= inner]


handleIssueCommentEventAction :: IssueCommentEvent -> (AppConfig -> IO ())
handleIssueCommentEventAction issueEvent = do
  maybe noop action $ isSupportedEvent issueEvent
  where

    action :: IssueCommentEvent -> (AppConfig -> IO ())
    action issueCommentEvent =
      if "robozd" == (whUserLogin . whIssueCommentUser . evIssueCommentPayload $ issueCommentEvent)
        then noop
        else doThingForComment issueCommentEvent


-- possible action values are created/edited/deleted/unknown. We only want to act on created or
-- edited and so we match those two and return the actual comment payload and we disregard anything
-- else by returning Nothing
isSupportedEvent :: IssueCommentEvent -> Maybe IssueCommentEvent
isSupportedEvent i@(evIssueCommentAction -> IssueCommentCreatedAction) = Just i
isSupportedEvent i@(evIssueCommentAction -> IssueCommentEditedAction)  = Just i
isSupportedEvent _                                                     = Nothing


doThingForComment :: IssueCommentEvent -> AppConfig -> IO ()
doThingForComment issueEvent config = do
  -- get the comment from the event payload and then check the comment body for links
  let comment = evIssueCommentPayload issueEvent

  -- get all links from the comment that look like they're clubhouse stories
  let allLinks = extractClubhouseLinks2 (whIssueCommentBody comment)

  -- check to see if the clubhouse links are real
  stories <- mapM (getStoryForLink config) allLinks
  (commentStories :: [Either StoryError Story]) <- traverse runExceptT stories
  let (_lefts, rights) = partitionEithers commentStories
  if null rights
    then logDebug ("No links found in this comment " <> getUrl (whIssueCommentHtmlUrl comment))
    else do
      mapM (either renderError renderStory) commentStories >>= print
    where
      renderError :: (Show a, MonadIO m) => a -> m T.Text
      renderError storyError = do
        let msg = T.pack $ show storyError
        logDebug msg
        pure msg

      renderStory :: Story -> IO T.Text
      renderStory (Story _ _ sName _) = do
        logDebug $ "Found: " <> sName
        pullRequestForIssueCommentEvent issueEvent config >>= \case
          Left err -> error $ show err
          Right pullRequestCommit -> do
            case traceShow pullRequestCommit (mkRequestParams pullRequestCommit) of
              Nothing -> undefined
              Just requestParams -> do
                let auth = GitHub.OAuth $ configGitHubToken config
                _ <- setHasStoryStatus' requestParams auth
                pure . T.pack $ show requestParams


mkRequestParams :: GitHub.PullRequestCommit -> Maybe GitHubRequestParams
mkRequestParams pullRequestCommit =
  let commit' = GitHub.mkCommitName $ GitHub.pullRequestCommitSha pullRequestCommit
  in case GitHub.pullRequestCommitRepo pullRequestCommit of
       Nothing -> Nothing
       Just repo' ->
         Just
           GitHubRequestParams
           { commit = commit'
           , owner = GitHub.simpleOwnerLogin (GitHub.repoOwner repo')
           , repo = GitHub.repoName repo'
           }


pullRequestForIssueCommentEvent ::
     IssueCommentEvent -> AppConfig -> IO (Either GitHub.Error GitHub.PullRequestCommit)
pullRequestForIssueCommentEvent issueEvent config =
  let hookIssue = evIssueCommentIssue issueEvent
      issueNumber = GitHub.IssueNumber $ whIssueNumber hookIssue
      repo' = evIssueCommentRepo issueEvent
      _repo = GitHub.mkRepoName (whRepoName repo')
      _owner = GitHub.mkOwnerName (either whSimplUserName whUserLogin (whRepoOwner repo'))
      auth = Just . GitHub.OAuth $ configGitHubToken config
  in (fmap . fmap) pullRequestHead (pullRequest' auth _owner _repo issueNumber)


fn ::
     (Traversable t, MonadIO m)
  => AppConfig
  -> t ClubhouseLink
  -> m (t (ExceptT StoryError IO Story))
fn config links = mapM (getStoryForLink config) links


fn2 :: Traversable t => AppConfig -> t ClubhouseLink -> IO (t (Either StoryError Story))
fn2 config links = fn config links >>= traverse runExceptT


resourceIdFromUrl :: URL -> Maybe (Id Int)
resourceIdFromUrl url =
  case parseURI strictURIParserOptions (encodeUtf8 (getUrl url)) of
    Left  _      -> Nothing
    Right parsed -> getId (C.split '/' (uriPath parsed))
     where
      getId (_ : _org : _repo : _resource : issueId : _) = do
        (i, _) <- C.readInt issueId
        Just $ Id i
      getId _ = Nothing
