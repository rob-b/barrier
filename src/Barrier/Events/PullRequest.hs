{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.PullRequest where

import           Barrier.Check                (extractClubhouseLinks, extractClubhouseLinks2)
import           Barrier.Clubhouse            (Story, getStory, mkClubhouseStoryUrl)
import           Barrier.Config               (AppConfig, readish)
import           Barrier.Events.Types         (WrappedHook (WrappedHookPullRequest),
                                               unWrapHookPullRequest)
import           Barrier.GitHub               (addStoryLinkComment, getCommentsForPullRequest,
                                               setHasStoryStatus, setMissingStoryStatus)
import           Control.Error                (runExceptT)
import           Control.Logger.Simple        (logDebug, logError)
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
import           GitHub.Data.Issues           (issueCommentBody)
import           GitHub.Data.Webhooks.Events  (PullRequestEvent, PullRequestEventAction (PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evPullReqAction, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whPullReqHead, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)
import           URI.ByteString               (Absolute, URIRef)


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
  let links = storyURI <> extractClubhouseLinks payload
  pure (setPullRequestStatus links unwrappedPayload)
  where
    convert
      :: Show a
      => Maybe a -> String -> [URIRef Absolute]
    convert Nothing _  = []
    convert (Just x) _ = either (const []) (\u -> [u]) (mkClubhouseStoryUrl x)


setPullRequestStatus :: [URIRef Absolute]
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
  -> URIRef Absolute
  -> [URIRef Absolute]
  -> IO ()
checkUpdatePullRequest config payload linkFromRefE linksFromPRDescE = do
  refStoryLink <- getStoryLink linkFromRefE

  storyLinksE <- mapM getStoryLink linksFromPRDescE
  (errors, stories) <- fmap partitionEithers (traverse runExceptT storyLinksE)

  issueCommentsE <- getCommentsForPullRequest config payload

  runExceptT refStoryLink >>= \case
    Right story  -> do
      setHasStoryStatus config payload story
      -- FIXME before adding the link we need to check that the comments don't already include a
      -- link. The PR data only includes the PR itself and so we will miss comments that include
      -- the link
      case issueCommentsE of
        Left err       -> logError $ T.pack ("Error retrieving comments: " <> show err)
        Right comments -> do

          let bodies = concatMap extractClubhouseLinks2 (fmap issueCommentBody comments)
          bodiesE <- mapM getStoryLink bodies
          (bodyErrors, bodyStories) <- fmap partitionEithers (traverse runExceptT bodiesE)

          mapM_ (logDebug . T.pack . show) bodyErrors

          -- no stories in either the description or the comments? Add a comment with the link
          when (null (stories <> bodyStories)) $ addStoryLinkComment config payload story

    -- we could not extract a story link from the commit ref, lets try getting a link from the PR
    -- description instead
    Left err -> do
      -- log the error encountered while extracting a link from the ref
      (logDebug . T.pack . show) err

      -- log any errors encountered while extract links from the description
      mapM_ (logDebug . T.pack . show) errors

      -- set an appropriate status for our list of stories
      _ <- checkerAndUpdater config payload stories
      pure ()
  where
    linkDebug link = logDebug ("Checking link: " <> T.pack (show link))

    -- getStoryLink (Left reason) = pure $ throwE $ StoryInvalidLinkError (show reason)
    getStoryLink link  = linkDebug link >> runReaderT (getStory link) config


checkerAndUpdater :: AppConfig
                  -> HookPullRequest
                  -> [Story]
                  -> IO ()
checkerAndUpdater config payload stories = do
  setStatus stories
  where
    setStatus (story:_) = setHasStoryStatus config payload story
    setStatus []        = setMissingStoryStatus config payload
