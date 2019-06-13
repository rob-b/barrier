{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barrier.Events where

import           Barrier.Config               (AppConfig)
import           Barrier.Events.Comment       (handleCommentEvent, handleIssueCommentEventAction)
import           Barrier.Events.PullRequest   (handlePullRequestAction, handlePullRequestEvent)
import           Barrier.Events.Types         (WrappedEvent (WrappedIssueComment, WrappedPullRequest))
import           Data.Aeson                   (ToJSON, Value, decodeStrict, encode)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (toStrict)
import           Data.Maybe                   (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid                  ((<>))
import qualified GitHub.Data.PullRequests     as GitHub
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent,
                                               evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest)


--------------------------------------------------------------------------------
supportedEvents :: [RepoWebhookEvent]
supportedEvents = [WebhookIssueCommentEvent, WebhookPullRequestEvent]


--------------------------------------------------------------------------------
-- | Given the value of the X-Github-Event header and the request body, select the appropriate
-- event type
selectEventType :: ByteString -> ByteString -> Maybe WrappedEvent
selectEventType eventHeader =
  decodeEventType (listToMaybe $ mapMaybe (`matchEvent` eventHeader) supportedEvents)


--------------------------------------------------------------------------------
-- | Given a RepoWebhookEvent and the bytestring value of X-Github-Event compare the bytestring
-- with the json encoded version of the RepoWebhookEvent to see if they are the same
matchEvent :: ToJSON a => a -> ByteString -> Maybe a
matchEvent event eventLabel
  | toStrict( encode event) == name' = Just event
  | otherwise = Nothing
  where name' = "\"" <> eventLabel <> "\""


--------------------------------------------------------------------------------
-- | Given an event type, build the appropriate response
selectResponse :: WrappedEvent -> Value
selectResponse (WrappedIssueComment issue) = handleCommentEvent issue
selectResponse (WrappedPullRequest pr)     = handlePullRequestEvent pr


--------------------------------------------------------------------------------
decodeEventType :: Maybe RepoWebhookEvent -> ByteString -> Maybe WrappedEvent
decodeEventType (Just WebhookPullRequestEvent) bs = WrappedPullRequest <$> (decodeStrict bs :: Maybe PullRequestEvent)
decodeEventType (Just WebhookIssueCommentEvent) bs = WrappedIssueComment <$> (decodeStrict bs :: Maybe IssueCommentEvent)
decodeEventType _ _ = Nothing


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
