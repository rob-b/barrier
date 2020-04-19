{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.Comment
  ( handleCommentEvent
  , handleIssueCommentEventAction
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
  maybe noop action $ grr issueEvent
  where

    action :: IssueCommentEvent -> (AppConfig -> IO ())
    action issueCommentEvent =
      if "robozd" == (whUserLogin . whIssueCommentUser . evIssueCommentPayload $ issueCommentEvent)
        then noop
        else doThingForComment issueCommentEvent


-- possible action values are created/edited/deleted/unknown. We only want to act on created or
-- edited and so we match those two and return the actual comment payload and we disregard anything
-- else by returning Nothing
grr :: IssueCommentEvent -> Maybe IssueCommentEvent
grr i@(evIssueCommentAction -> IssueCommentCreatedAction) = Just i
grr i@(evIssueCommentAction -> IssueCommentEditedAction)  = Just i
grr _                                                     = Nothing


doThingForComment :: IssueCommentEvent -> AppConfig -> IO ()
doThingForComment issueEvent config = do
  -- get the comment from the event payload and then check the comment body for links
  let comment = evIssueCommentPayload issueEvent
  let allLinks = extractClubhouseLinks2 (whIssueCommentBody comment)
  stories <- mapM (getStoryForLink config) allLinks
  (commentStories :: [Either StoryError Story]) <- traverse runExceptT stories
  when
    (null (rights commentStories))
    (logDebug ("No links found in this comment " <> getUrl (whIssueCommentHtmlUrl comment)))
  _ <- mapM_ xo commentStories
  pure ()
    where
      xo (Left a)                    = logDebug $ "No story found: " <> T.pack (show a)
      xo (Right (Story _ _ sName _)) = logDebug $ "Found: " <> sName


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
