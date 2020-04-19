{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barrier.Handlers.GitHub
  ( EventBody(EventBody)
  , EventHeader(EventHeader)
  , UnsupportedEvent(UnsupportedEvent)
  , selectAction
  , selectEventType
  , selectResponse
  ) where

import           Barrier.Config               (AppConfig)
import           Barrier.Events.Comment       (handleCommentEvent, handleIssueCommentEventAction)
import           Barrier.Events.PullRequest   (handlePullRequestAction, handlePullRequestEvent)
import           Barrier.Events.Types
    (WrappedEvent(WrappedIssueComment, WrappedPullRequest))
import           Data.Aeson
    (Value(String), decodeStrict, eitherDecode, eitherDecodeStrict, encode)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (toStrict)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.String.Conversions      (convertString)
import           Data.Text.Encoding           (decodeUtf8)
import qualified GitHub.Data.PullRequests     as GitHub
import           GitHub.Data.Webhooks
    (RepoWebhookEvent(WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events
    (IssueCommentEvent, PullRequestEvent, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest)


newtype EventHeader = EventHeader { unEventHeader :: ByteString }
newtype EventBody = EventBody { unEventBody :: ByteString }
newtype UnsupportedEvent = UnsupportedEvent { unUnsupportedEvent :: ByteString } deriving (Show)


--------------------------------------------------------------------------------
-- | Given the value of the X-Github-Event header and the request body, select the appropriate
-- event type
selectEventType :: EventHeader -> EventBody -> Either UnsupportedEvent WrappedEvent
selectEventType eventHeader eventBody =
  either Left (`decodeEventType` eventBody) (eventFromHeaderValue eventHeader)


--------------------------------------------------------------------------------
eventFromHeaderValue :: EventHeader -> Either UnsupportedEvent RepoWebhookEvent
eventFromHeaderValue (EventHeader header) =
  case eitherDecode . encode . String . decodeUtf8 $ header of
    (Right WebhookPullRequestEvent)  -> Right WebhookPullRequestEvent
    (Right WebhookIssueCommentEvent) -> Right WebhookIssueCommentEvent
    (Right x)                        -> Left $ UnsupportedEvent  $ toStrict $ encode x
    (Left x)                         -> Left . UnsupportedEvent $ convertString x


--------------------------------------------------------------------------------
decodeEventType :: RepoWebhookEvent -> EventBody -> Either UnsupportedEvent WrappedEvent
decodeEventType WebhookIssueCommentEvent (EventBody bs) =
  case eitherDecodeStrict bs of
    Left err -> Left $ UnsupportedEvent $ convertString err
    Right x  -> Right $ WrappedIssueComment x
decodeEventType WebhookPullRequestEvent (EventBody bs) =
  case eitherDecodeStrict bs of
    Left err -> Left $ UnsupportedEvent $ convertString err
    Right x  -> Right $ WrappedPullRequest x
decodeEventType _ _ =
  Left $ UnsupportedEvent "Only pull-request and issue comment events are supported."


--------------------------------------------------------------------------------
-- | Given an event type, build the appropriate response
selectResponse :: WrappedEvent -> Value
selectResponse (WrappedIssueComment issue) = handleCommentEvent issue
selectResponse (WrappedPullRequest pr)     = handlePullRequestEvent pr


--------------------------------------------------------------------------------
-- | Given an event from GH, select the appropriate response handler
selectAction :: WrappedEvent -> (AppConfig -> IO ())
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


--------------------------------------------------------------------------------
issueCommentEventFromFixture :: IO (Maybe IssueCommentEvent)
issueCommentEventFromFixture = do
  fromMaybe (error "cannot decode issue_comment_created.json") . decodeStrict <$>
    readFixture "issue_comment_created.json"
