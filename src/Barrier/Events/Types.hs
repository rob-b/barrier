{-# LANGUAGE DataKinds #-}

module Barrier.Events.Types
  ( unWrapHookPullRequest
  , WrappedEvent(WrappedIssueComment, WrappedPullRequest)
  , WrappedHook
  ) where

import           Data.WorldPeace              (OpenUnion, catchesOpenUnion)
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent)
import           GitHub.Data.Webhooks.Payload (HookIssueComment, HookPullRequest)


data WrappedEvent
  = WrappedPullRequest PullRequestEvent
  | WrappedIssueComment IssueCommentEvent
  deriving (Show)


type WrappedHook = OpenUnion '[HookPullRequest, HookIssueComment]


unWrapHookPullRequest :: WrappedHook -> Maybe HookPullRequest
unWrapHookPullRequest = catchesOpenUnion (Just, const Nothing)
