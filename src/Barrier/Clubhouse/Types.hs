{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Barrier.Clubhouse.Types
  ( ClubhouseLink(ClubhouseLink)
  , Story(Story, storyUrl)
  , StoryError(StoryHttpError, StoryNotFoundError, StoryParseError)
  , ClubhouseAction
  , ClubhouseActionEntityType(ChaStory)
  , ClubhouseEvent(ClubhouseEvent)
  , EventParseError(EventParseError)
  , ExpandedAction(ExpandedAction)
  , WorkflowStateOptions(ClubhouseWorkflowChanged,
                     ClubhouseWorkflowCreated)
  , chActionEntityType
  , chActionId
  , chActionName
  , chActionWorkflowState
  , chActions
  , chNewState
  , chOldState
  , decodeChEvent
  , decodeChReferences
  , getWorkFlowId
  ) where

import           Data.Aeson
    ( FromJSON
    , ToJSON
    , eitherDecodeStrict
    , genericToJSON
    , parseJSON
    , toJSON
    , withObject
    , withText
    , (.:)
    , (.:?)
    )
import           Data.Aeson.BetterErrors
    (Parse, asIntegral, asText, key, keyMay, toAesonParser)
import qualified Data.Aeson.BetterErrors          as BetterErrors
import           Data.Aeson.BetterErrors.Internal (ErrorSpecifics(FromAeson), badSchema)
import           Data.Aeson.Casing                (aesonDrop, snakeCase)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as C
import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IntMap
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           GHC.Generics                     (Generic)
import           URI.ByteString
    (Absolute, URIRef, parseURI, strictURIParserOptions)


--------------------------------------------------------------------------------
data ExpandedAction = ExpandedAction
  { title    :: T.Text
  , id       :: Int
  , oldState :: Maybe T.Text
  , newState :: Maybe T.Text
  } deriving (Show, Generic)


--------------------------------------------------------------------------------
instance ToJSON ExpandedAction where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase


--------------------------------------------------------------------------------
newtype ClubhouseLink = ClubhouseLink
  { unClubhouseLink :: URIRef Absolute
  } deriving (Eq, Show)


--------------------------------------------------------------------------------
data Story = Story
  { storyType :: !Text
  , storyId   :: !Int
  , storyName :: !Text
  , storyUrl  :: !(URIRef Absolute)
  } deriving (Eq, Ord, Show, Generic)


--------------------------------------------------------------------------------
newtype CustomParseError =
  UrlParseError String
  deriving (Show)


--------------------------------------------------------------------------------
parseURL :: Text -> Either CustomParseError (URIRef Absolute)
parseURL url =
  case parseURI strictURIParserOptions (encodeUtf8 url) of
    Left err    -> Left $ UrlParseError (show err)
    Right value -> Right value


--------------------------------------------------------------------------------
displayCustomParseError :: CustomParseError -> Text
displayCustomParseError (UrlParseError reason) = "Cannot parse url: " <> T.pack reason


--------------------------------------------------------------------------------
asStory :: Parse CustomParseError Story
asStory = Story <$> key "story_type" asText <*> key "id" asIntegral <*> key "name" asText <*> key "app_url" (BetterErrors.withText parseURL)


--------------------------------------------------------------------------------
instance FromJSON Story where
  parseJSON = toAesonParser displayCustomParseError asStory


--------------------------------------------------------------------------------
data StoryError
  = StoryParseError String
  | StoryNotFoundError
  | StoryHttpError String
  | StoryInvalidLinkError String
  deriving (Show)


--------------------------------------------------------------------------------
data ClubhouseReference = ClubhouseReference
  { chReferenceId         :: Int
  , chReferenceEntityType :: ClubhouseReferenceEntityType
  , chReferenceType       :: Maybe Text
  , chReferenceName       :: Text
  } deriving (Show, Generic)


--------------------------------------------------------------------------------
newtype ClubhouseReferences = ClubhouseReferences
  { chReferences :: [ClubhouseReference]
  } deriving (Show, Generic)


--------------------------------------------------------------------------------
instance FromJSON ClubhouseReferences where
  parseJSON = withObject "references" $ \o -> do ClubhouseReferences <$> o .: "references"


--------------------------------------------------------------------------------
instance FromJSON ClubhouseReference where
  parseJSON = withObject "reference" $ \o -> do
    chReferenceId <- o .: "id"
    chReferenceEntityType <- o .: "entity_type"
    chReferenceType <- o .:? "type"
    chReferenceName <- o .: "name"
    pure ClubhouseReference { .. }


--------------------------------------------------------------------------------
data ClubhouseReferenceEntityType
  = ClubhouseWorkflowEntity
  | UnsupportedEntity
  deriving (Show, Generic)


--------------------------------------------------------------------------------
instance FromJSON ClubhouseReferenceEntityType where
  parseJSON =
    withText "entity_type" $ \case
      "workflow-state" -> pure ClubhouseWorkflowEntity
      _ -> pure UnsupportedEntity


--------------------------------------------------------------------------------
newtype ClubhouseEvent = ClubhouseEvent
  { chActions :: [ClubhouseAction]
  } deriving (Show, Generic)


--------------------------------------------------------------------------------
instance FromJSON ClubhouseEvent where
  parseJSON = BetterErrors.toAesonParser clubhouseEventErrorToString asClubhouseEvent
    where clubhouseEventErrorToString _ = "Received \"null\" value"


--------------------------------------------------------------------------------
data ClubhouseEventError = NullValue deriving Show


--------------------------------------------------------------------------------
asClubhouseEvent :: Parse ClubhouseEventError ClubhouseEvent
asClubhouseEvent = do
  let parser = ClubhouseEvent <$> key "actions" (BetterErrors.eachInArray asClubhouseAction)
  BetterErrors.perhaps parser >>= \case
    Nothing -> badSchema $ BetterErrors.CustomError NullValue
    Just v  -> pure v


--------------------------------------------------------------------------------
instance ToJSON ClubhouseEvent where
  toJSON = genericToJSON $ aesonDrop 2 snakeCase


--------------------------------------------------------------------------------
data ClubhouseActionEntityType
  = ChaStory
  | ChaPullRequest
  | ChaUnknown
  deriving (Show, Generic)


--------------------------------------------------------------------------------
parseEntityType :: Text -> Either a2 ClubhouseActionEntityType
parseEntityType t = case t of
  "story"        -> Right ChaStory
  "pull-request" -> Right ChaPullRequest
  _              -> Right ChaUnknown


--------------------------------------------------------------------------------
instance ToJSON ClubhouseActionEntityType where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase


--------------------------------------------------------------------------------
data ClubhouseAction = ClubhouseAction
  { chActionId            :: Int
  , chActionAction        :: Text
  , chActionEntityType    :: ClubhouseActionEntityType
  , chActionName          :: Maybe Text
  , chActionWorkflowState :: WorkflowStateOptions
  } deriving (Show, Generic)


--------------------------------------------------------------------------------
asClubhouseAction :: Parse e ClubhouseAction
asClubhouseAction =
  ClubhouseAction <$> key "id" asIntegral <*> key "action" asText <*> key "entity_type" (BetterErrors.withText parseEntityType) <*> keyMay "name" asText <*> asWorkflowStateOptions


--------------------------------------------------------------------------------
asWorkflowStateOptions :: Parse e WorkflowStateOptions
asWorkflowStateOptions = do
  action <- key "action" asText
  case action of
    "update" -> ClubhouseWorkflowChanged <$> key "changes" asClubhouseWorkflowState
    "create" -> ClubhouseWorkflowCreated <$> key "workflow_state_id" asIntegral
    _        -> badSchema (FromAeson $ "Invalid action value: " <> T.unpack action)


--------------------------------------------------------------------------------
instance FromJSON ClubhouseAction where
  parseJSON = BetterErrors.toAesonParser' asClubhouseAction


--------------------------------------------------------------------------------
instance ToJSON ClubhouseAction where
  toJSON = genericToJSON $ aesonDrop 8 snakeCase


--------------------------------------------------------------------------------
newtype WorkFlowId = WorkFlowId
  { getWorkFlowId :: Int
  } deriving (Generic, Show, Real, Num, Ord, Eq, Enum, Integral)


--------------------------------------------------------------------------------
data WorkflowStateOptions
  = ClubhouseWorkflowChanged ClubhouseWorkflowState
  | ClubhouseWorkflowCreated WorkFlowId
  deriving (Show, Generic)


--------------------------------------------------------------------------------
data ClubhouseWorkflowState = ClubhouseWorkflowState
  { chOldState :: WorkFlowId
  , chNewState :: WorkFlowId
  } deriving (Show, Generic)



instance ToJSON WorkflowStateOptions where
  toJSON  = genericToJSON $ aesonDrop 2 snakeCase


instance FromJSON WorkflowStateOptions where
  parseJSON = BetterErrors.toAesonParser' asWorkflowStateOptions

instance ToJSON WorkFlowId where
  toJSON  = genericToJSON $ aesonDrop 3 snakeCase


--------------------------------------------------------------------------------
instance FromJSON ClubhouseWorkflowState where
  parseJSON = BetterErrors.toAesonParser' asClubhouseWorkflowState


--------------------------------------------------------------------------------
instance ToJSON ClubhouseWorkflowState where
  toJSON = genericToJSON $ aesonDrop 2 snakeCase


--------------------------------------------------------------------------------
asClubhouseWorkflowState :: Parse e ClubhouseWorkflowState
asClubhouseWorkflowState =
  ClubhouseWorkflowState <$> key "workflow_state_id" (key "old" asIntegral) <*> key "workflow_state_id" (key "new" asIntegral)


--------------------------------------------------------------------------------
readEvent :: IO (Either EventParseError ClubhouseEvent)
readEvent = do
  let jsonData = C.readFile "clubhouse-event.json"
  decodeChEvent <$> jsonData


--------------------------------------------------------------------------------
readReferences :: IO (Either String (IntMap Text))
readReferences = do
  decodeChReferences <$> C.readFile "clubhouse-event.json"


newtype EventParseError =
  EventParseError String
  deriving (Show)


--------------------------------------------------------------------------------
decodeChEvent :: ByteString -> Either EventParseError ClubhouseEvent
decodeChEvent bs =
  case eitherDecodeStrict bs of
    Left err    -> Left $ EventParseError err
    Right event -> Right event


--------------------------------------------------------------------------------
decodeChReferences :: ByteString -> Either String (IntMap Text)
decodeChReferences bs = do
  refs <- eitherDecodeStrict bs
  pure . foldRefs $ mapper <$> filter predicate (chReferences refs)
  where
    foldRefs refs = foldl IntMap.union IntMap.empty refs
    predicate ref = case chReferenceEntityType ref of
         ClubhouseWorkflowEntity -> True
         _                       -> False
    mapper :: ClubhouseReference -> IntMap Text
    mapper ref = IntMap.fromList [(chReferenceId ref, chReferenceName ref)]


--------------------------------------------------------------------------------
decodeChWorkflowState :: ByteString -> Either String ClubhouseWorkflowState
decodeChWorkflowState = eitherDecodeStrict
