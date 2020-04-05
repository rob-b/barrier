{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barrier.Handlers.GitHub where

import           Barrier.Config               (AppConfig)
import           Barrier.Events.Comment       (handleCommentEvent, handleIssueCommentEventAction)
import           Barrier.Events.PullRequest   (handlePullRequestAction, handlePullRequestEvent)
import           Barrier.Events.Types
    (WrappedEvent(WrappedIssueComment, WrappedPullRequest))
import           Data.Aeson                   (Value(String), decode, decodeStrict, encode)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text.Encoding           (decodeUtf8)
import qualified GitHub.Data.PullRequests     as GitHub
import           GitHub.Data.Webhooks
    (RepoWebhookEvent(WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events
    (IssueCommentEvent, PullRequestEvent, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest)

newtype EventHeader = EventHeader { unEventHeader :: ByteString }
newtype EventBody = EventBody { unEventBody :: ByteString }


--------------------------------------------------------------------------------
-- Given the X-GitHub-Event header and the body of a github webhook select an action to run
getActionFromEvent :: EventHeader -> EventBody -> Maybe (AppConfig -> IO())
getActionFromEvent header body = selectEventType header body >>= selectAction


--------------------------------------------------------------------------------
-- | Given the value of the X-Github-Event header and the request body, select the appropriate
-- event type
selectEventType :: EventHeader -> EventBody -> Maybe WrappedEvent
selectEventType eventHeader eventBody =
  eventFromHeaderValue eventHeader >>= (`decodeEventType` eventBody)


--------------------------------------------------------------------------------
eventFromHeaderValue :: EventHeader -> Maybe RepoWebhookEvent
eventFromHeaderValue (EventHeader header) = case decode . encode . String . decodeUtf8 $ header of
  a@(Just WebhookPullRequestEvent)  -> a
  a@(Just WebhookIssueCommentEvent) -> a
  _                                 -> Nothing


--------------------------------------------------------------------------------
decodeEventType :: RepoWebhookEvent -> EventBody -> Maybe WrappedEvent
decodeEventType WebhookPullRequestEvent (EventBody bs)  = WrappedPullRequest <$> decodeStrict bs
decodeEventType WebhookIssueCommentEvent (EventBody bs) = WrappedIssueComment <$> decodeStrict bs
decodeEventType _ _                                     = Nothing


--------------------------------------------------------------------------------
-- | Given an event type, build the appropriate response
selectResponse :: WrappedEvent -> Value
selectResponse (WrappedIssueComment issue) = handleCommentEvent issue
selectResponse (WrappedPullRequest pr)     = handlePullRequestEvent pr


--------------------------------------------------------------------------------
-- | Given an event from GH, select the appropriate response handler
selectAction :: WrappedEvent -> Maybe (AppConfig -> IO ())
selectAction (WrappedPullRequest pr)     = handlePullRequestAction pr
selectAction (WrappedIssueComment issue) = handleIssueCommentEventAction issue


--------------------------------------------------------------------------------
readFixture :: FilePath -> IO ByteString
readFixture name = B.readFile $ "fixtures/" <> name


--------------------------------------------------------------------------------
eventFromFixture :: IO PullRequestEvent
eventFromFixture = do
  fromMaybe (error "cannot decode zd_pull_request_event.json") . decodeStrict <$> readFixture "zd_pull_request_event.json"


--------------------------------------------------------------------------------
payloadFromFixture :: IO HookPullRequest
payloadFromFixture = evPullReqPayload <$> eventFromFixture


--------------------------------------------------------------------------------
pullRequestPayloadFromFixture :: IO GitHub.PullRequest
pullRequestPayloadFromFixture = do
  fromMaybe (error "cannot decode pull_request_payload.json") . decodeStrict <$> readFixture "pull_request_payload.json"



issueCommentEventFromFixture :: IO (Maybe IssueCommentEvent)
issueCommentEventFromFixture = do
  fromMaybe (error "cannot decode issue_comment_created.json") . decodeStrict <$>
    readFixture "issue_comment_created.json"
