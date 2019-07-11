{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.PullRequest where

import           Barrier.Check                (extractClubhouseLinks, extractClubhouseLinks2)
import           Barrier.Clubhouse            (ClubhouseLink, Story, StoryError (StoryHttpError),
                                               getStory, mkClubhouseStoryUrl)
import           Barrier.Config               (AppConfig, readish)
import           Barrier.Events.Types         (WrappedHook (WrappedHookPullRequest),
                                               unWrapHookPullRequest)
import           Barrier.GitHub               (addStoryLinkComment, getCommentsForPullRequest,
                                               setHasStoryStatus, setMissingStoryStatus)
import           Control.Error                (ExceptT, runExceptT)
import           Control.Logger.Simple        (logDebug)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (runReaderT)
import           Data.Aeson                   (Value, object, (.=))
import           Data.Either                  (lefts)
import           Data.Maybe                   (listToMaybe, maybeToList)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           GitHub.Data.Issues           (IssueComment, issueCommentBody)
import           GitHub.Data.Webhooks.Events  (PullRequestEvent, PullRequestEventAction (PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction),
                                               evPullReqAction, evPullReqPayload)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, getUrl, whPullReqHead,
                                               whPullReqHtmlUrl, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)


--------------------------------------------------------------------------------
-- | Given an PullRequestEvent, extract its inner payload
getPayLoadFromPr :: PullRequestEvent -> Maybe WrappedHook
getPayLoadFromPr pr@(evPullReqAction -> PullRequestOpenedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestEditedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(evPullReqAction -> PullRequestReopenedAction) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr pr@(syncCheck -> True) = Just $ WrappedHookPullRequest (evPullReqPayload pr)
getPayLoadFromPr _ = Nothing


--------------------------------------------------------------------------------
-- | Build the json body for the response to an incoming PullRequestEvent
handlePullRequestEvent :: PullRequestEvent -> Value
handlePullRequestEvent event =
  let inner = V.singleton $ object ["base" .= (ref :: Text)]
      ref = maybe "Dunno." (whPullReqTargetRef . whPullReqHead) payload
      payload = unWrapHookPullRequest <$> getPayLoadFromPr event
  in object ["data" .= inner]


--------------------------------------------------------------------------------
-- | Select the appropriate action to perform on an incoming PullRequestEvent
handlePullRequestAction :: PullRequestEvent -> Maybe (AppConfig -> IO ())
handlePullRequestAction pr = do
  payload <- getPayLoadFromPr pr
  pure $ setPullRequestStatus payload


--------------------------------------------------------------------------------
getStoryLinkFromPayload :: HookPullRequest -> Maybe ClubhouseLink
getStoryLinkFromPayload payload = do
  let targetRef = whPullReqTargetRef . whPullReqHead $ payload
  convert (extractStoryId targetRef) (T.unpack targetRef)
  where
    convert :: Show a => Maybe a -> String -> Maybe ClubhouseLink
    convert Nothing _  = Nothing
    convert (Just x) _ = either (const Nothing) Just (mkClubhouseStoryUrl x)


--------------------------------------------------------------------------------
setPullRequestStatus :: WrappedHook -> AppConfig -> IO ()
setPullRequestStatus hook config = do
  let payload  = unWrapHookPullRequest hook
  resultE <- getStoriesOrDieTrying config hook
  stories <- case resultE of
    Left errors   -> do
      logDebug $ "No story could be detected from " <> getUrl (whPullReqHtmlUrl payload)
      mapM_ (logDebug . T.pack . show) errors
      pure []
    Right stories -> pure stories
  checkerAndUpdater config payload stories


--------------------------------------------------------------------------------
-- | Handle the special case of a "synchronize" PullRequestEvent
syncCheck :: PullRequestEvent -> Bool
syncCheck event =
  case evPullReqAction event of
    (PullRequestActionOther "synchronize") -> True
    _                                      -> False


--------------------------------------------------------------------------------
extractStoryId :: Text -> Maybe Int
extractStoryId value = extract ((listToMaybe . scan regex) value) >>= readish
  where
    regex = [re|^.*(ch(\d+)).*$|]
    extract (Just (_, _:ref:_)) = Just ref
    extract _                   = Nothing


--------------------------------------------------------------------------------
checkerAndUpdater :: AppConfig
                  -> HookPullRequest
                  -> [FoundStory]
                  -> IO ()
checkerAndUpdater config payload stories = do
  setStatus stories
  where
    commentsOnly fs =
      case foundSource fs of
        CommentSource     -> True
        DescriptionSource -> True
        RefSource         -> False

    setStatus [] = setMissingStoryStatus config payload
    setStatus stories'@(story:_) = do
      let story' = foundStory story
      setHasStoryStatus config payload story'
      unless (any commentsOnly stories') (addStoryLinkComment config payload story')


--------------------------------------------------------------------------------
sequenceEithers :: [Either a b] -> Either [a] [b]
sequenceEithers xs =
    case sequence xs of
      Right values -> Right values
      Left _       -> Left (lefts xs)


data StorySource
  = RefSource
  | DescriptionSource
  | CommentSource
  deriving (Show)


data FoundStory = FoundStory
  { foundSource :: StorySource
  , foundStory  :: Story
  }


--------------------------------------------------------------------------------
getStoriesOrDieTrying ::
     AppConfig -> WrappedHook -> IO (Either [StoryError] [FoundStory])
getStoriesOrDieTrying config hook = do
  let payload = unWrapHookPullRequest hook

  -- try to get a link to a story by parsing the payload itself (the branch name is in the
  -- payload)
  let storyFromRef = maybeToList $ getStoryLinkFromPayload payload

  -- try to get link(s) to the story from the PR description
  let links = extractClubhouseLinks hook

  -- stories gathered from the PR refname
  s <- mapM (getStoryForLink config) storyFromRef
  refStory <- traverse runExceptT s

  -- stories gathered from the PR description
  ss <- mapM (getStoryForLink config) links
  descStories <- traverse runExceptT ss

  -- stories gathered from the PR comments
  issueCommentsE <- getCommentsForPullRequest config payload
  issueComments <-
    case issueCommentsE of
      Left err    -> pure [Left (StoryHttpError (show err))]
      Right comms -> storiesFromComments comms

  pure $
    sequenceEithers
      (fmap convertToRef refStory <> fmap convertToDescription descStories <>
       fmap convertToComment issueComments)

  where
    storiesFromComments :: V.Vector IssueComment -> IO [Either StoryError Story]
    storiesFromComments comments = do
      let bodies = concatMap extractClubhouseLinks2 (fmap issueCommentBody comments)
      bodiesE <- mapM (getStoryForLink config) bodies
      traverse runExceptT bodiesE


    convertToDescription = fmap (FoundStory DescriptionSource)
    convertToComment = fmap (FoundStory CommentSource)
    convertToRef = fmap (FoundStory RefSource)


linkDebug :: (Show a, MonadIO m) => a -> m ()
linkDebug link = logDebug ("Checking link: " <> T.pack (show link))

getStoryForLink :: (MonadIO m) => AppConfig -> ClubhouseLink -> m (ExceptT StoryError IO Story)
getStoryForLink config l = linkDebug l >> runReaderT (getStory l) config
