module Barrier.Events.Types where

import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent)
import           GitHub.Data.Webhooks.Payload (HookIssueComment, HookPullRequest)

data WrappedEvent
  = WrappedPullRequest { unWrapPullRequest :: PullRequestEvent }
  | WrappedIssueComment { unWrappIssueComment :: IssueCommentEvent }
  deriving (Show)


data WrappedHook
  = WrappedHookPullRequest { unWrapHookPullRequest :: HookPullRequest}
  | WrappedHookIssueComment { unWrapHookIssueComment :: HookIssueComment}
  deriving (Show)
