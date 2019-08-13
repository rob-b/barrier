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
import           GitHub.Data.Webhooks.Payload (whIssueCommentBody, whIssueCommentUser, whUserLogin)


handleCommentEvent :: IssueCommentEvent -> Value
handleCommentEvent event =
  let inner = V.singleton $ object ["comment" .= comment]
      comment = whIssueCommentBody $ evIssueCommentPayload event
  in object ["data" .= inner]


handleIssueCommentEventAction :: IssueCommentEvent -> Maybe (AppConfig -> IO ())
handleIssueCommentEventAction issue = do
  wrappedHook <- getWrappedHookFromIssue issue
  if whUserLogin (whIssueCommentUser (evIssueCommentPayload issue)) == "robozd"
    then Nothing
    else Just $ doThingForComment wrappedHook


getWrappedHookFromIssue :: IssueCommentEvent -> Maybe WrappedHook
getWrappedHookFromIssue issue@(evIssueCommentAction -> IssueCommentCreatedAction) =
  Just . WrappedHookIssueComment $ evIssueCommentPayload issue
getWrappedHookFromIssue _ = Nothing


doThingForComment :: WrappedHook -> AppConfig -> IO ()
doThingForComment hook _config = do
  let allLinks = extractClubhouseLinks hook
  if null allLinks
    then logDebug "No links found in this comment"
    else do
      let msg = "At this point we should do something for these links" ++ show allLinks
      logDebug $ T.pack msg
