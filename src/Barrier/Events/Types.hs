{-# LANGUAGE OverloadedStrings #-}
module Barrier.Events.Types where

import           Barrier.Check                (filterByDomain)
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent)
import           GitHub.Data.Webhooks.Payload (HookIssueComment, HookPullRequest,
                                               whIssueCommentBody, whPullReqBody)
import           URI.ByteString               (Absolute, URIParseError, URIRef)

data WrappedEvent
  = WrappedPullRequest { unWrapPullRequest :: PullRequestEvent }
  | WrappedIssueComment { unWrappIssueComment :: IssueCommentEvent }
  deriving (Show)


data WrappedHook
  = WrappedHookPullRequest { unWrapHookPullRequest :: HookPullRequest}
  | WrappedHookIssueComment { unWrapHookIssueComment :: HookIssueComment}
  deriving (Show)


extractLinks :: WrappedHook -> [Either URIParseError (URIRef Absolute)]
extractLinks hook =
  let body
        | (WrappedHookIssueComment inner) <- hook = whIssueCommentBody inner
        | (WrappedHookPullRequest inner) <- hook = whPullReqBody inner
        | otherwise = ""
  in filterByDomain body "app.clubhouse.io"
