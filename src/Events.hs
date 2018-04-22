{-# LANGUAGE OverloadedStrings #-}
module Events where
import           Data.Aeson                   (ToJSON, Value (Object), decode, encode, object,
                                               (.=))
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Vector                  as V
import           GHC.Exts                     (fromList)
import           GitHub.Data.Webhooks         (RepoWebhookEvent (WebhookIssueCommentEvent, WebhookPullRequestEvent))
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent, evIssueCommentPayload)
import           GitHub.Data.Webhooks.Payload (whIssueCommentBody)


selectEventType :: ByteString -> Maybe RepoWebhookEvent
selectEventType event' =
  let match = listToMaybe $ catMaybes $ fmap (`matchEvent` event') events
  in match


selectResponse :: Maybe RepoWebhookEvent -> ByteString -> Either Value Value
selectResponse (Just WebhookIssueCommentEvent) bs = Right $ handleCommentEvent bs
selectResponse (Just x) _ =
  Left (Object $ fromList ["error" .= (("Handler not added for event: " <> show x) :: String)])
selectResponse Nothing _ =
  Left (Object $ fromList ["error" .= ("Unsupported event: event" :: Value)])


handleCommentEvent :: ByteString -> Value
handleCommentEvent bs =
  let ev = decode (fromStrict bs) :: Maybe IssueCommentEvent
  in case ev of
       Nothing ->
         let inner = V.singleton $ object ["detail" .= ("Failed to parse event" :: String)]
         in object ["errors" .= inner]
       Just ev' ->
         let inner = V.singleton $ object ["comment" .= comment]
             comment = whIssueCommentBody $ evIssueCommentPayload ev'
         in object ["data" .= inner]


events :: [RepoWebhookEvent]
events = [WebhookIssueCommentEvent, WebhookPullRequestEvent]


matchEvent :: ToJSON a => a -> ByteString -> Maybe a
matchEvent event eventLabel
  | toStrict( encode event) == name' = Just event
  | otherwise = Nothing
  where name' = "\"" <> eventLabel <> "\""
