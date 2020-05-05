{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Barrier.Events.PullRequest
  ( handlePullRequestAction
  , handlePullRequestEvent
  ) where


import           Barrier.Actions              (getStoryForLink, sequenceEithers)
import           Barrier.Check                (extractClubhouseLinks, extractClubhouseLinks2)
import           Barrier.Clubhouse            (mkClubhouseStoryUrl)
import           Barrier.Clubhouse.Types      (ClubhouseLink, Story, StoryError(StoryHttpError))
import           Barrier.Config               (AppConfig, readish)
import           Barrier.Events               (noop)
import           Barrier.Events.Types         (WrappedHook, unWrapHookPullRequest)
import           Barrier.GitHub
    (addStoryLinkComment, getCommentsForPullRequest, setHasStoryStatus, setMissingStoryStatus)
import           Barrier.ListUtils            (ordNub)
import           Control.Error                (runExceptT)
import           Control.Logger.Simple        (logDebug)
import           Control.Monad                (unless)
import           Data.Aeson                   (Value, object, (.=))
import           Data.Maybe                   (listToMaybe, maybeToList)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Data.WorldPeace              (openUnionLift)
import           GitHub.Data.Issues           (IssueComment, issueCommentBody)
import           GitHub.Data.Webhooks.Events
    ( PullRequestEvent
    , PullRequestEventAction(PullRequestActionOther, PullRequestEditedAction, PullRequestOpenedAction, PullRequestReopenedAction)
    , evPullReqAction
    , evPullReqPayload
    )
import           GitHub.Data.Webhooks.Payload
    (HookPullRequest, getUrl, whPullReqHead, whPullReqHtmlUrl, whPullReqTargetRef)
import           Text.Regex.PCRE.Heavy        (re, scan)


--------------------------------------------------------------------------------
-- | Given an PullRequestEvent, extract its inner payload
getPullRequestFromEvent :: PullRequestEvent -> Maybe WrappedHook
getPullRequestFromEvent pr@(evPullReqAction -> PullRequestOpenedAction) = Just $ openUnionLift (evPullReqPayload pr)
getPullRequestFromEvent pr@(evPullReqAction -> PullRequestEditedAction) = Just $ openUnionLift (evPullReqPayload pr)
getPullRequestFromEvent pr@(evPullReqAction -> PullRequestReopenedAction) = Just $ openUnionLift (evPullReqPayload pr)
getPullRequestFromEvent pr@(syncCheck -> True) = Just $ openUnionLift (evPullReqPayload pr)
getPullRequestFromEvent _ = Nothing


--------------------------------------------------------------------------------
-- | Build the json body for the response to an incoming PullRequestEvent
handlePullRequestEvent :: PullRequestEvent -> Value
handlePullRequestEvent event =
  let inner = V.singleton $ object ["base" .= (ref :: Text)]
      ref = maybe "Dunno." (whPullReqTargetRef . whPullReqHead) payload
      payload = getPullRequestFromEvent event >>= unWrapHookPullRequest
  in object ["data" .= inner]


--------------------------------------------------------------------------------
-- | Select the appropriate action to perform on an incoming PullRequestEvent
handlePullRequestAction :: PullRequestEvent -> AppConfig -> IO ()
handlePullRequestAction pr = maybe noop setPullRequestStatus (getPullRequestFromEvent pr)


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
setPullRequestStatus wrappedHook config =
  case unWrapHookPullRequest wrappedHook of
    Nothing              -> pure ()
    Just hookPullRequest -> setPullRequestStatus' hookPullRequest config
  where
    setPullRequestStatus' :: HookPullRequest -> AppConfig -> IO ()
    setPullRequestStatus' payload config' = do
      resultE <- getStoriesOrDieTrying config' payload
      stories <-
        case resultE of
          Left errors -> do
            logDebug $ "No story could be detected from " <> getUrl (whPullReqHtmlUrl payload)
            mapM_ (logDebug . T.pack . show) errors
            pure []
          Right stories -> pure $ ordNub stories
      logDebug . T.pack $ show stories
      checkerAndUpdater config' payload stories


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
      setHasStoryStatus config payload
      unless (any commentsOnly stories') (addStoryLinkComment config payload story')


data StorySource
  = RefSource
  | DescriptionSource
  | CommentSource
  deriving (Show, Eq, Ord)


data FoundStory = FoundStory
  { foundSource :: StorySource
  , foundStory  :: Story
  } deriving (Show, Ord, Eq)


--------------------------------------------------------------------------------
getStoriesOrDieTrying ::
     AppConfig -> HookPullRequest -> IO (Either [StoryError] [FoundStory])
getStoriesOrDieTrying config hook = do
  let payload = hook

  -- try to get a link to a story by parsing the payload itself (the branch name is in the
  -- payload)
  let storyFromRef :: [ClubhouseLink] = maybeToList $ getStoryLinkFromPayload payload

  -- try to get link(s) to the story from the PR description (ignoring any links we already found)
  let links :: [ClubhouseLink] = filter (`notElem` storyFromRef) (extractClubhouseLinks hook)

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
