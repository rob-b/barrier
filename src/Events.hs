{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Events where


import           Data.Aeson                   (ToJSON, Value, decode, encode, object, (.=))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Maybe                   (catMaybes, fromMaybe, listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Text.Read               (decimal)
import qualified Data.Vector                  as V
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent,
                                               PullRequestEventAction (PullRequestOpenedAction),
                                               evIssueCommentPayload, evPullReqAction,
                                               evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whIssueCommentBody, whPullReqHead,
                                               whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)


data WrappedEvent
  = WrappedPullRequest { unWrapPullRequest :: PullRequestEvent }
  | WrappedIssueComment { unWrappIssueComment :: IssueCommentEvent }
  deriving (Show)



supportedEvents :: [RepoWebhookEvent]
supportedEvents = [WebhookIssueCommentEvent, WebhookPullRequestEvent]


-- | Given the value of the X-Github-Event header and the request body, select the appropriate
-- event type
selectEventType :: ByteString -> ByteString -> Maybe WrappedEvent
selectEventType eventHeader eventBody =
  decodeEventType
    (listToMaybe $ catMaybes $ fmap (`matchEvent` eventHeader) supportedEvents)
    eventBody


-- | Given a RepoWebhookEvent and the bytestring value of X-Github-Event compare the bytestring
-- with the json encoded version of the RepoWebhookEvent to see if they are the same
matchEvent :: ToJSON a => a -> ByteString -> Maybe a
matchEvent event eventLabel
  | toStrict( encode event) == name' = Just event
  | otherwise = Nothing
  where name' = "\"" <> eventLabel <> "\""


-- | Given an event type, build the appropriate response
selectResponse :: WrappedEvent -> Value
selectResponse (WrappedIssueComment issue) = handleCommentEvent issue
selectResponse (WrappedPullRequest pr)     = handlePullRequestEvent pr


decodeEventType :: Maybe RepoWebhookEvent -> ByteString -> Maybe WrappedEvent
decodeEventType (Just WebhookPullRequestEvent) bs =
  WrappedPullRequest <$> (decode (fromStrict bs) :: Maybe PullRequestEvent)
decodeEventType _ _ = Nothing


handleCommentEvent :: IssueCommentEvent -> Value
handleCommentEvent event =
  let inner = V.singleton $ object ["comment" .= comment]
      comment = whIssueCommentBody $ evIssueCommentPayload event
  in object ["data" .= inner]


handlePullRequestEvent :: PullRequestEvent -> Value
handlePullRequestEvent event =
  let inner = V.singleton $ object ["base" .= (ref :: Text)]
      ref = maybe "dunno" (whPullReqTargetRef . whPullReqHead) branch
      branch = getBranchFromNewPr event
  in object ["data" .= inner]


selectAction :: WrappedEvent -> Maybe (IO ())
selectAction (WrappedPullRequest pr) = handlePullRequestAction pr
selectAction _                       = Nothing


handlePullRequestAction :: PullRequestEvent -> Maybe (IO ())
handlePullRequestAction pr =
  let (idM :: Maybe Int) =
        extractStoryId (whPullReqTargetRef . whPullReqHead $ evPullReqPayload pr)
      x = mkClubhouseStoryUrl <$> idM
      content = fromMaybe "Cannot extract story id" x
  in Just (T.appendFile "example.txt" (content <> "\n"))


getBranchFromNewPr :: PullRequestEvent -> Maybe HookPullRequest
getBranchFromNewPr pr@(evPullReqAction -> PullRequestOpenedAction) = Just $ evPullReqPayload pr
getBranchFromNewPr _                                               = Nothing


extractStoryId :: Integral a => Text -> Maybe a
extractStoryId value = extract ((listToMaybe . scan regex) value) >>= readish
  where
    regex = [re|^.*(ch(\d+)).*$|]
    extract (Just (_, _:ref:_)) = Just ref
    extract _                   = Nothing


readish :: Integral a => Text -> Maybe a
readish s = either (const Nothing) (Just . fst) (decimal s)


mkClubhouseStoryUrl :: Show a => a -> Text
mkClubhouseStoryUrl storyID =
  T.intercalate
    ""
    [ "https://api.clubhouse.io/api/v2/stories/"
    , T.pack (show storyID)
    , "?token=$CLUBHOUSE_API_TOKEN"
    ]


readFixture :: FilePath -> IO ByteString
readFixture name = B.readFile $ "fixtures/" <> name
