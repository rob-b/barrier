{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.PullRequest where

import           Barrier.Check                (extractClubhouseLinks)
import           Barrier.Clubhouse            (Story, StoryError (StoryInvalidLinkError), getStory,
                                               mkClubhouseStoryUrl)
import           Barrier.Config               (AppConfig, readish)
import           Barrier.Events.Types         (WrappedHook (WrappedHookPullRequest),
                                               unWrapHookPullRequest)
import           Barrier.GitHub               (addStoryLinkComment, getCommentsForPullRequest,
                                               setHasStoryStatus, setMissingStoryStatus)
import           Control.Error                (runExceptT, throwE)
import           Control.Logger.Simple        (logDebug)
import           Control.Monad                (when)
import           Control.Monad.Reader         (runReaderT)
import           Data.Aeson                   (Value, object, (.=))
import           Data.Either                  (partitionEithers)
import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Debug.Trace                  (trace)
import           GitHub.Data.Webhooks.Events  (PullRequestEvent, PullRequestEventAction (PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evPullReqAction, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whPullReqHead, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)
import           URI.ByteString               (Absolute, URIParseError (OtherError), URIRef)


-- | Given an PullRequestEvent, extract its inner payload
getPayLoadFromPr :: PullRequestEvent -> Maybe WrappedHook
getPayLoadFromPr pr@(evPullReqAction -> PullRequestOpenedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestEditedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestReopenedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(syncCheck -> True) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr _ = Nothing


-- | Build the json body for the response to an incoming PullRequestEvent
handlePullRequestEvent :: PullRequestEvent -> Value
handlePullRequestEvent event =
  let inner = V.singleton $ object ["base" .= (ref :: Text)]
      ref = maybe "Dunno." (whPullReqTargetRef . whPullReqHead) payload
      payload = unWrapHookPullRequest <$> getPayLoadFromPr event
  in object ["data" .= inner]


-- | Select the appropriate action to perform on an incoming PullRequestEvent
handlePullRequestAction :: PullRequestEvent -> Maybe (AppConfig -> IO ())
handlePullRequestAction pr = do
  payload <- getPayLoadFromPr pr
  let unwrappedPayload = unWrapHookPullRequest payload
  let targetRef = whPullReqTargetRef . whPullReqHead $ unwrappedPayload
  let storyURI = convert (extractStoryId targetRef) (T.unpack targetRef)
  let links = [storyURI] <> extractClubhouseLinks payload
  pure (setPullRequestStatus links unwrappedPayload)
  where
    convert
      :: Show a
      => Maybe a -> String -> Either URIParseError (URIRef Absolute)
    convert Nothing ref =
      Left $ OtherError ("Could not determine story id from branch name: " <> ref)
    convert (Just x) _ = mkClubhouseStoryUrl x


setPullRequestStatus :: [Either URIParseError (URIRef Absolute)]
                     -> HookPullRequest
                     -> AppConfig
                     -> IO ()
setPullRequestStatus [] payload config = setMissingStoryStatus config payload
setPullRequestStatus (link:links) payload config =
  trace "checking and updating" (checkUpdatePullRequest config payload link links)


-- | Handle the special case of a "synchronize" PullRequestEvent
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


checkUpdatePullRequest
  :: AppConfig
  -> HookPullRequest
  -> Either URIParseError (URIRef Absolute)
  -> [Either URIParseError (URIRef Absolute)]
  -> IO ()
checkUpdatePullRequest config payload linkFromRefE linksFromBodyE = do
  refStoryLink <- getStoryLink linkFromRefE

  storyLinksE <- mapM getStoryLink linksFromBodyE
  (errors, stories) <- fmap partitionEithers (traverse runExceptT storyLinksE)

  let issueCommentsE = getCommentsForPullRequest config payload

  runExceptT refStoryLink >>= \case
    Right story  -> do
      setHasStoryStatus config payload story
      -- FIXME before adding the link we need to check that the comments don't already include a
      -- link. The PR data only includes the PR itself and so we will miss comments that include
      -- the link
      when (null stories) $ addStoryLinkComment config payload story
    Left err -> do
      (logDebug . T.pack . show) err
      mapM_ (logDebug . T.pack . show) errors
      _ <- checkerAndUpdater config payload stories
      pure ()
  where
    getStoryLink (Left reason) = pure $ throwE $ StoryInvalidLinkError (show reason)
    getStoryLink (Right link)  = linkDebug link >> runReaderT (getStory link) config

    linkDebug link = logDebug ("Checking link: " <> T.pack (show link))


checkerAndUpdater :: AppConfig
                  -> HookPullRequest
                  -> [Story]
                  -> IO ()
checkerAndUpdater config payload stories = do
  setStatus stories
  where
    setStatus (story:_) = setHasStoryStatus config payload story
    setStatus []        = setMissingStoryStatus config payload
