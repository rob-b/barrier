{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events where

import           Barrier.Check                (filterByDomain)
import           Barrier.Clubhouse            (StoryError (StoryInvalidLinkError), getStory,
                                               mkClubhouseStoryUrl)
import           Barrier.Config               (AppConfig, readish)
import           Barrier.GitHub               (setHasStoryStatus, setMissingStoryStatus)
import           Control.Error                (runExceptT, throwE)
import           Control.Logger.Simple        (logDebug)
import           Control.Monad                (mapM)
import           Control.Monad.Reader         (runReaderT)
import           Data.Aeson                   (ToJSON, Value, decode, encode, object, (.=))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Either                  (partitionEithers)
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Debug.Trace                  (trace, traceShow)
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, PullRequestEvent, PullRequestEventAction (PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evIssueCommentPayload, evPullReqAction,
                                               evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whIssueCommentBody, whPullReqBody,
                                               whPullReqHead, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)
import           URI.ByteString               (Absolute, URIParseError (OtherError), URIRef)


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
  let targetRef = whPullReqTargetRef . whPullReqHead $ payload
  let storyURI = convert (extractStoryId targetRef) (T.unpack targetRef)
  let links = [storyURI] <> extractLinks payload
  if traceShow links (null links)
    then pure (`setMissingStoryStatus` payload)
    else pure (\config -> trace "checking and updating" (checkerAndUpdater config payload links))
  where
    convert
      :: Show a
      => Maybe a -> String -> Either URIParseError (URIRef Absolute)
    convert Nothing ref =
      Left $ OtherError ("Could not determine story id from branch name: " <> ref)
    convert (Just x) _ = mkClubhouseStoryUrl x


checkUpdateComment :: AppConfig
                   -> HookPullRequest
                   -> [Either URIParseError (URIRef Absolute)]
                   -> IO ()
checkUpdateComment config payload links = do
  _ <- checkerAndUpdater config payload links
  case mayTail links of
    Nothing     -> pure ()
    Just links' -> addLink config payload links'
  where
    mayTail xs =
      if null xs
        then Nothing
        else Just (tail xs)


addLink :: AppConfig -> HookPullRequest -> [Either URIParseError (URIRef Absolute)] -> IO ()
addLink config payload links = undefined


checkerAndUpdater :: AppConfig
                  -> HookPullRequest
                  -> [Either URIParseError (URIRef Absolute)]
                  -> IO ()
checkerAndUpdater config payload links = do
  responses <- mapM doThingWithLink links
  (errors, stories) <- fmap partitionEithers (traverse runExceptT responses)
  mapM_ (logDebug . T.pack . show) errors
  selectStatus stories
  where
    selectStatus (story:_) = setHasStoryStatus config payload story
    selectStatus []        = setMissingStoryStatus config payload

    doThingWithLink (Left reason) = pure $ throwE $ StoryInvalidLinkError (show reason)
    doThingWithLink (Right link)  = linkDebug link >> runReaderT (getStory link) config

    linkDebug link = logDebug ("Checking link: " <> T.pack (show link))


extractLinks :: HookPullRequest -> [Either URIParseError (URIRef Absolute)]
extractLinks pr = filterByDomain (whPullReqBody pr) "app.clubhouse.io"


getPayLoadFromPr :: PullRequestEvent -> Maybe HookPullRequest
getPayLoadFromPr pr@(evPullReqAction -> PullRequestOpenedAction)   = Just $ evPullReqPayload pr
getPayLoadFromPr pr@(evPullReqAction -> PullRequestEditedAction)   = Just $ evPullReqPayload pr
getPayLoadFromPr pr@(evPullReqAction -> PullRequestReopenedAction) = Just $ evPullReqPayload pr
getPayLoadFromPr pr@(syncCheck -> True)                            = Just $ evPullReqPayload pr
getPayLoadFromPr _                                                 = Nothing


syncCheck :: PullRequestEvent -> Bool
syncCheck event =
  case evPullReqAction event of
    (PullRequestActionOther "synchronize") -> True
    _                                      -> False


extractStoryId :: Text -> Maybe Int
extractStoryId value = extract ((listToMaybe . scan regex) value) >>= readish
  where
    regex = [re|^.*(ch(\d+)).*$|]
    extract (Just (_, _:ref:_)) = Just ref
    extract _                   = Nothing


readFixture :: FilePath -> IO ByteString
readFixture name = B.readFile $ "fixtures/" <> name


eventFromFixture :: IO PullRequestEvent
eventFromFixture = do
  f <- readFixture "pull_request_event.json"
  let (Just event) = selectEventType "pull_request" f
  pure $ unWrapPullRequest event


payloadFromFixture :: IO HookPullRequest
payloadFromFixture = evPullReqPayload <$> eventFromFixture
