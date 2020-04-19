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
  maybe noop action $ getIssueCommentFromEvent issueEvent
  where
    action :: HookIssueComment -> (AppConfig -> IO ())
    action comment =
      if whUserLogin (whIssueCommentUser comment) == "robozd"
        then noop
        else doThingForComment (evIssueCommentIssue issueEvent) comment

-- whIssueUrl $ evIssueCommentIssue issueCommentEvent


getIssueCommentFromEvent :: IssueCommentEvent -> Maybe HookIssueComment
getIssueCommentFromEvent issue@(evIssueCommentAction -> IssueCommentCreatedAction) =
  Just $ evIssueCommentPayload issue
getIssueCommentFromEvent issue@(evIssueCommentAction -> IssueCommentEditedAction) =
  Just $ evIssueCommentPayload issue
getIssueCommentFromEvent _ = Nothing


doThingForComment :: a -> HookIssueComment -> AppConfig -> IO ()
doThingForComment _issue comment config = do
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
