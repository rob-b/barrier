{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events where


import           Barrier.Check                (filterByDomain)
import           Barrier.Clubhouse            (getStory, mkClubhouseStoryUrl)
import           Barrier.Config               (AppConfig, configClubhouseToken)
import           Barrier.GitHub               (setMissingStoryStatus)
import           Control.Error                (hush, runExceptT)
import           Control.Monad                (forM_)
import           Control.Monad.Reader         (runReaderT)
import           Data.Aeson                   (ToJSON, Value, decode, encode, object, (.=))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Maybe                   (catMaybes, listToMaybe, maybeToList)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Data.Text.Read               (decimal)
import qualified Data.Vector                  as V
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent, PullRequestEventAction (PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evIssueCommentPayload, evPullReqAction,
                                               evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whIssueCommentBody, whPullReqBody,
                                               whPullReqHead, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)
import           URI.ByteString               (Absolute, URIRef)


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
      ref = maybe "dunno" (whPullReqTargetRef . whPullReqHead) payload
      payload = getPayLoadFromPr event
  in object ["data" .= inner]


selectAction :: WrappedEvent -> Maybe (AppConfig -> IO ())
selectAction (WrappedPullRequest pr) = handlePullRequestAction pr
selectAction _                       = Nothing


handlePullRequestAction :: PullRequestEvent -> Maybe (AppConfig -> IO ())
handlePullRequestAction pr = do
  payload <- getPayLoadFromPr pr
  let urlM =
        (hush . mkClubhouseStoryUrl) =<<
        (extractStoryId . whPullReqTargetRef . whPullReqHead $ payload)
  let links = extractLinks payload <> maybeToList urlM
  if null links
    then pure (`setMissingStoryStatus` payload)
    else pure (\config -> checkerAndUpdater config payload links)


checkerAndUpdater :: Foldable t => AppConfig -> HookPullRequest -> t (URIRef Absolute) -> IO ()
checkerAndUpdater config _payload links = forM_ links $ \link -> do
  let _chToken = configClubhouseToken config
  xx <- runReaderT (getStory link) config
  runExceptT xx >>= \case
    Left reason -> print reason
    Right value -> print value


extractLinks :: HookPullRequest -> [URIRef Absolute]
extractLinks pr = filterByDomain (whPullReqBody pr) "clubhouse"


getPayLoadFromPr :: PullRequestEvent -> Maybe HookPullRequest
getPayLoadFromPr pr@(evPullReqAction -> PullRequestOpenedAction)   = Just $ evPullReqPayload pr
getPayLoadFromPr pr@(evPullReqAction -> PullRequestEditedAction)   = Just $ evPullReqPayload pr
getPayLoadFromPr pr@(evPullReqAction -> PullRequestReopenedAction) = Just $ evPullReqPayload pr
getPayLoadFromPr _                                                 = Nothing


extractStoryId :: Text -> Maybe Int
extractStoryId value = extract ((listToMaybe . scan regex) value) >>= readish
  where
    regex = [re|^.*(ch(\d+)).*$|]
    extract (Just (_, _:ref:_)) = Just ref
    extract _                   = Nothing


readish :: Integral a => Text -> Maybe a
readish s = either (const Nothing) (Just . fst) (decimal s)


readFixture :: FilePath -> IO ByteString
readFixture name = B.readFile $ "fixtures/" <> name


eventFromFixture :: IO PullRequestEvent
eventFromFixture = do
  f <- readFixture "pull_request_event.json"
  let (Just event) = selectEventType "pull_request" f
  pure $ unWrapPullRequest event


payloadFromFixture :: IO HookPullRequest
payloadFromFixture = evPullReqPayload <$> eventFromFixture
