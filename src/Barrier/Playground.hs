module Barrier.Playground where

import           Data.Aeson                   (decodeStrict, eitherDecodeStrict)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import qualified GitHub.Data.PullRequests     as GitHub
import           GitHub.Data.Webhooks.Events
    (IssueCommentEvent, PullRequestEvent, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest)

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
  fromMaybe (error "cannot decode pull_request_payload.json") . decodeStrict <$>
    readFixture "pull_request_payload.json"


--------------------------------------------------------------------------------
issueCommentEventFromFixture :: IO (Either String IssueCommentEvent)
issueCommentEventFromFixture = do
   eitherDecodeStrict <$> readFixture "issue_comment_created_with_null_body.json"
