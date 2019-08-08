{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Barrier.Events.Comment where


import           Barrier.Check                (extractClubhouseLinks)
import           Barrier.Config               (AppConfig)
import           Barrier.Events.Types         (WrappedHook (WrappedHookIssueComment))
import           Control.Logger.Simple        (logDebug)
import           Data.Aeson                   (Value, object, (.=))
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
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
  pure $ doThingForComment payload


getPayLoadFromIssue :: IssueCommentEvent -> Maybe WrappedHook
getPayLoadFromIssue issue@(evIssueCommentAction -> IssueCommentCreatedAction) =
  Just . WrappedHookIssueComment $ evIssueCommentPayload issue
getPayLoadFromIssue _ = Nothing


doThingForComment :: WrappedHook -> AppConfig -> IO ()
doThingForComment hook _config = do
  let allLinks = extractClubhouseLinks hook
  logDebug . T.pack $ show allLinks
  logDebug "doing thing for comment"
