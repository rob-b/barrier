{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Barrier.Events.Comment where


import           Barrier.Config               (AppConfig)
import           Barrier.Events.Types         (WrappedHook (WrappedHookIssueComment), extractLinks)
import           Data.Aeson                   (Value, object, (.=))
import           Data.Either                  (rights)
import qualified Data.Vector                  as V
import           Debug.Trace                  (traceShow)
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent,
                                               IssueCommentEventAction (IssueCommentCreatedAction),
                                               evIssueCommentAction, evIssueCommentPayload)
import           GitHub.Data.Webhooks.Payload (whIssueCommentBody)


handleCommentEvent :: IssueCommentEvent -> Value
handleCommentEvent event =
  let inner = V.singleton $ object ["comment" .= comment]
      comment = whIssueCommentBody $ evIssueCommentPayload event
  in object ["data" .= inner]


handleIssueCommentEventAction :: IssueCommentEvent -> Maybe (AppConfig -> IO ())
handleIssueCommentEventAction issue = do
  payload <- getPayLoadFromIssue issue
  let allLinks = extractLinks payload
  let links = traceShow allLinks (rights allLinks)
  traceShow links $ pure undefined


getPayLoadFromIssue :: IssueCommentEvent -> Maybe WrappedHook
getPayLoadFromIssue issue@(evIssueCommentAction -> IssueCommentCreatedAction) =
  Just . WrappedHookIssueComment $ evIssueCommentPayload issue
getPayLoadFromIssue _ = Nothing
