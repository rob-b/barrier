{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.Comment
  ( handleCommentEvent
  , handleIssueCommentEventAction
  , isSupportedEvent
  , doThingForComment
  ) where


import           Barrier.Actions               (getStoryForLink)
import           Barrier.Check                 (extractClubhouseLinks2)
import           Barrier.Clubhouse.Types       (ClubhouseLink, Story(Story), StoryError)
import           Barrier.Config                (AppConfig, configGitHubToken)
import           Barrier.Events                (noop)
import           Control.Error                 (ExceptT, runExceptT)
import           Control.Logger.Simple         (logDebug)
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (Value, object, (.=))
import qualified Data.ByteString.Char8         as C
import           Data.Either                   (rights)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Vector                   as V
import           Debug.Trace
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
  let comment = traceShow (evIssueCommentPayload issueEvent) (evIssueCommentPayload issueEvent)
  let allLinks = extractClubhouseLinks2 (whIssueCommentBody comment)
  stories <- mapM (getStoryForLink config) allLinks
  (commentStories :: [Either StoryError Story]) <- traverse runExceptT stories
  let _xx = traceShow commentStories commentStories
  when
    (null (rights commentStories))
    (logDebug ("No links found in this comment " <> getUrl (whIssueCommentHtmlUrl comment)))
  _ <- mapM_ xo commentStories
  pure ()
    where
      xo (Left a)                    = logDebug $ "No story found: " <> T.pack (show a)
      xo (Right (Story _ _ sName _)) = logDebug $ "Found: " <> sName


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
