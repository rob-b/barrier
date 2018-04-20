{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lib
   where

import Data.Text (Text)
import           Control.Logger.Simple
import           Data.Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8
import           Data.ByteString.Lazy                 (toStrict, fromStrict)
import           Data.HVect                           (HVect ((:&:), HNil))
import           Data.Maybe                           (catMaybes, fromMaybe, listToMaybe)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           GitHub.Data.Webhooks
import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload
import           GitHub.Data.Webhooks.Secure          (isSecurePayload)
import           Network.HTTP.Types.Status            (status201, status401, status422)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)
import           Web.Spock                            (SpockActionCtx, SpockM, body, getContext,
                                                       getState, header, jsonBody, middleware,
                                                       post, prehook, rawHeader, root, runSpock,
                                                       setStatus, spock, text, get)
import           Web.Spock.Config                     (PoolOrConn (PCNoDatabase), defaultSpockCfg)


logger = logStdoutDev

data User = User

newtype AppState = AppState
  { appStateToken :: ByteString
  }


type Api = SpockM () () AppState ()
type ApiAction a = SpockActionCtx (HVect '[]) () () AppState a
type AuthedApiAction ctx a = SpockActionCtx ctx () () AppState a


app :: Api
app = prehook initHook $ do
  prehook authHook $ post root $ do
      event <- body
      eventKind <- rawHeader "X-Github-Event"
      let value = (`encodeEvent` event) =<< eventKind
      logDebug (T.pack $ show value)
      text $ selectResponse value event
  get "/a" $ do
      text "rocking it"


initHook :: AuthedApiAction () (HVect '[])
initHook = return HNil


authHook :: AuthedApiAction (HVect xs) (HVect (User ': xs))
authHook = do
  oldCtx <- getContext
  appState <- getState
  payload <- body
  logDebug (decodeUtf8 payload)
  signature <- header "X-Hub-Signature"
  if isSecurePayload (decodeUtf8 $ appStateToken appState) signature payload
    then return (User :&: oldCtx)
    else do
      setStatus status401
      text "get lost"


-- barrierConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
-- barrierConfig cfg = cfg { spc_errorHandler = errorHandler }


run :: IO ()
run =
  withGlobalLogging (LogConfig (Just "logfile.txt") False) $ do
    key <- maybe mempty C8.pack <$> lookupEnv "GITHUB_KEY"
    let appState = AppState key
    spockCfg <- defaultSpockCfg () PCNoDatabase appState
    runSpock 9000 (spock spockCfg $ middleware logger >> app)


data Events = IssueComment IssueCommentEvent | Issues IssuesEvent | PullRequest PullRequestEvent

encodeEvent :: ByteString -> p -> Maybe RepoWebhookEvent
encodeEvent event' _bs =
  let match = listToMaybe $ catMaybes $ fmap (`matchEvent` event') events
  in match

selectResponse :: Maybe RepoWebhookEvent -> ByteString -> Text
selectResponse (Just WebhookIssueCommentEvent) bs = do
  let ev = decode (fromStrict bs) :: Maybe IssueCommentEvent
  maybe "oops" (whIssueCommentBody . evIssueCommentPayload) ev
selectResponse _ _ = "Look yeah"


events :: [RepoWebhookEvent]
events = [WebhookIssueCommentEvent, WebhookIssuesEvent, WebhookPullRequestEvent]


matchEvent :: ToJSON a => a -> ByteString -> Maybe a
matchEvent event eventLabel
  | toStrict( encode event) == name' = Just event
  | otherwise = Nothing
  where name' = "\"" <> eventLabel <> "\""
