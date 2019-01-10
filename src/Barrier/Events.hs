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
import           Barrier.GitHub               (addStoryLinkComment, setHasStoryStatus,
                                               setMissingStoryStatus)
import           Control.Error                (runExceptT, throwE)
import           Control.Logger.Simple        (logDebug)
import           Control.Monad                (mapM)
import           Control.Monad.Reader         (runReaderT)
import           Data.Aeson                   (ToJSON, Value, decodeStrict, encode, object, (.=))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.ByteString.Lazy         (toStrict)
import           Data.Either                  (partitionEithers, rights)
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Debug.Trace
import           Debug.Trace                  (trace, traceShow)
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent,
                                               IssueCommentEventAction (IssueCommentCreatedAction),
                                               PullRequestEvent,
                                               PullRequestEventAction (PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evIssueCommentAction, evIssueCommentPayload,
                                               evPullReqAction, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookIssueComment, HookPullRequest,
                                               whIssueCommentBody, whPullReqBody, whPullReqHead,
                                               whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)
import           URI.ByteString               (Absolute, URIParseError (OtherError), URIRef)


data WrappedEvent
  = WrappedPullRequest { unWrapPullRequest :: PullRequestEvent }
  | WrappedIssueComment { unWrappIssueComment :: IssueCommentEvent }
  deriving (Show)


data WrappedHook
  = WrappedHookPullRequest { unWrapHookPullRequest :: HookPullRequest}
  | WrappedHookIssueComment { unWrapHookIssueComment :: HookIssueComment}
  deriving (Show)


supportedEvents :: [RepoWebhookEvent]
supportedEvents = [WebhookIssueCommentEvent, WebhookPullRequestEvent]


-- | Given the value of the X-Github-Event header and the request body, select the appropriate
-- event type
selectEventType :: ByteString -> ByteString -> Maybe WrappedEvent
selectEventType eventHeader =
  decodeEventType (listToMaybe $ catMaybes $ fmap (`matchEvent` eventHeader) supportedEvents)


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
decodeEventType (Just WebhookPullRequestEvent) bs = WrappedPullRequest <$> (decodeStrict bs :: Maybe PullRequestEvent)
decodeEventType (Just WebhookIssueCommentEvent) bs = WrappedIssueComment <$> (decodeStrict bs :: Maybe IssueCommentEvent)
decodeEventType _ _ = Nothing


handleCommentEvent :: IssueCommentEvent -> Value
handleCommentEvent event =
  let inner = V.singleton $ object ["comment" .= comment]
      comment = whIssueCommentBody $ evIssueCommentPayload event
  in object ["data" .= inner]


handlePullRequestEvent :: PullRequestEvent -> Value
handlePullRequestEvent event =
  let inner = V.singleton $ object ["base" .= (ref :: Text)]
      ref = maybe "Dunno." (whPullReqTargetRef . whPullReqHead) payload
      payload = unWrapHookPullRequest <$> getPayLoadFromPr event
  in object ["data" .= inner]


selectAction :: WrappedEvent -> Maybe (AppConfig -> IO ())
selectAction (WrappedPullRequest pr)     = handlePullRequestAction pr
selectAction (WrappedIssueComment issue) = handleIssueCommentEventAction issue
selectAction _                           = Nothing


handleIssueCommentEventAction :: IssueCommentEvent -> Maybe (AppConfig -> IO ())
handleIssueCommentEventAction issue = do
  payload <- getPayLoadFromIssue issue
  let links = rights $ extractLinks payload
  traceShow links $ pure undefined


handlePullRequestAction :: PullRequestEvent -> Maybe (AppConfig -> IO ())
handlePullRequestAction pr = do
  payload <- getPayLoadFromPr pr
  let unwrappedPayload = unWrapHookPullRequest payload
  let targetRef = whPullReqTargetRef . whPullReqHead $ unwrappedPayload
  let storyURI = convert (extractStoryId targetRef) (T.unpack targetRef)
  let links = [storyURI] <> extractLinks payload
  pure (duppy links unwrappedPayload)
  where
    convert
      :: Show a
      => Maybe a -> String -> Either URIParseError (URIRef Absolute)
    convert Nothing ref =
      Left $ OtherError ("Could not determine story id from branch name: " <> ref)
    convert (Just x) _ = mkClubhouseStoryUrl x


duppy :: [Either URIParseError (URIRef Absolute)] -> HookPullRequest -> AppConfig -> IO ()
duppy [] payload config = setMissingStoryStatus config payload
duppy (link:links) payload config =
  trace "checking and updating" (checkUpdateComment config payload link links)


checkUpdateComment :: AppConfig
                   -> HookPullRequest
                   -> Either URIParseError (URIRef Absolute)
                   -> [Either URIParseError (URIRef Absolute)]
                   -> IO ()
checkUpdateComment config payload linkFromRefE linksFromBodyE = do
  _ <- checkerAndUpdater config payload ([linkFromRefE] <> linksFromBodyE)
  addLink config payload linksFromBodyE


addLink :: AppConfig -> HookPullRequest -> [Either URIParseError (URIRef Absolute)] -> IO ()
addLink config payload links = addStoryLinkComment config payload links


checkerAndUpdater :: AppConfig
                  -> HookPullRequest
                  -> [Either URIParseError (URIRef Absolute)]
                  -> IO ()
checkerAndUpdater config payload linksE = do
  storyLinksE <- mapM getStoryLinkFromPR linksE
  (errors, stories) <- fmap partitionEithers (traverse runExceptT storyLinksE)
  mapM_ (logDebug . T.pack . show) errors
  selectStatus stories
  where
    selectStatus (story:_) = setHasStoryStatus config payload story
    selectStatus []        = setMissingStoryStatus config payload

    getStoryLinkFromPR (Left reason) = pure $ throwE $ StoryInvalidLinkError (show reason)
    getStoryLinkFromPR (Right link)  = linkDebug link >> runReaderT (getStory link) config

    linkDebug link = logDebug ("Checking link: " <> T.pack (show link))


extractLinks :: WrappedHook -> [Either URIParseError (URIRef Absolute)]
extractLinks hook =
  let body
        | (WrappedHookIssueComment inner) <- hook = whIssueCommentBody inner
        | (WrappedHookPullRequest inner) <- hook = whPullReqBody inner
        | otherwise = ""
  in filterByDomain body "app.clubhouse.io"


getPayLoadFromPr :: PullRequestEvent -> Maybe WrappedHook
getPayLoadFromPr pr@(evPullReqAction -> PullRequestOpenedAction) =
  Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestEditedAction) =
  Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestReopenedAction) =
  Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(syncCheck -> True) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr _ = Nothing


getPayLoadFromIssue :: IssueCommentEvent -> Maybe WrappedHook
getPayLoadFromIssue issue@(evIssueCommentAction -> IssueCommentCreatedAction) =
  Just . WrappedHookIssueComment $ evIssueCommentPayload issue
getPayLoadFromIssue _ = Nothing


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
