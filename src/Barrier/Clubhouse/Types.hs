{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barrier.Clubhouse.Types where

import           Data.Aeson            (FromJSON, ToJSON, eitherDecodeStrict, genericToJSON,
                                        parseJSON, toJSON, withObject, withText, (.:), (.:?))
import           Data.Aeson.Casing     (aesonDrop, snakeCase)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IntMap
import           Data.Text             (Text)
import           GHC.Generics          (Generic)

import           Data.Text.Encoding    (encodeUtf8)
import           URI.ByteString        (Absolute, URIRef, parseURI, strictURIParserOptions)


newtype ClubhouseLink = ClubhouseLink
  { unClubhouseLink :: URIRef Absolute
  } deriving (Show)


data Story = Story
  { storyType :: !Text
  , storyId   :: !Int
  , storyName :: !Text
  , storyUrl  :: !(URIRef Absolute)
  } deriving (Eq, Ord, Show, Generic)


instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> do
    url <- o .: "app_url"
    case parseURI strictURIParserOptions (encodeUtf8 url) of
      Left err -> fail (show err)
      Right value -> do
        storyType <- o .: "story_type"
        storyId <- o .: "id"
        storyName <- o .: "name"
        let storyUrl = value
        pure Story {..}


data StoryError
  = StoryParseError String
  | StoryNotFoundError
  | StoryHttpError String
  | StoryInvalidLinkError String
  deriving (Show)


data ClubhouseReference = ClubhouseReference
  { chReferenceId         :: Int
  , chReferenceEntityType :: ClubhouseReferenceEntityType
  , chReferenceType       :: Maybe Text
  , chReferenceName       :: Text
  } deriving (Show, Generic)


newtype ClubhouseReferences = ClubhouseReferences
  { chReferences :: [ClubhouseReference]
  } deriving (Show, Generic)


instance FromJSON ClubhouseReferences where
  parseJSON = withObject "references" $ \o -> do ClubhouseReferences <$> o .: "references"


instance FromJSON ClubhouseReference where
  parseJSON = withObject "reference" $ \o -> do
    chReferenceId <- o .: "id"
    chReferenceEntityType <- o .: "entity_type"
    chReferenceType <- o .:? "type"
    chReferenceName <- o .: "name"
    pure ClubhouseReference { .. }


data ClubhouseReferenceEntityType
  = ClubhouseWorkflowEntity
  | UnsupportedEntity
  deriving (Show, Generic)


instance FromJSON ClubhouseReferenceEntityType where
  parseJSON =
    withText "entity_type" $ \case
      "workflow-state" -> pure ClubhouseWorkflowEntity
      _ -> pure UnsupportedEntity


newtype ClubhouseEvent = ClubhouseEvent
  { chActions :: [ClubhouseAction]
  } deriving (Show, Generic)


instance FromJSON ClubhouseEvent where
  parseJSON = withObject "event" $ \o -> do ClubhouseEvent <$> o .: "actions"


instance ToJSON ClubhouseEvent where
  toJSON = genericToJSON $ aesonDrop 2 snakeCase


data ClubhouseAction = ClubhouseAction
  { chActionId            :: Int
  , chActionAction        :: Text
  , chActionEntityType    :: Text
  , chActionName          :: Text
  , chActionWorkflowState :: ClubhouseWorkflowState
  } deriving (Show, Generic)


instance FromJSON ClubhouseAction where
  parseJSON = withObject "story" $ \o -> do
    chActionId <- o .: "id"
    chActionAction <- o .: "action"
    chActionEntityType <- o .: "entity_type"
    chActionName <- o .: "name"
    chActionWorkflowState <- parseWorkflow o
    pure ClubhouseAction { .. }

    where
      parseWorkflow o = do
        changes <- o .: "changes"
        stateObject <- changes .: "workflow_state_id"
        chOldState <- stateObject .: "old"
        chNewState <- stateObject .: "new"
        pure ClubhouseWorkflowState { .. }


instance ToJSON ClubhouseAction where
  toJSON = genericToJSON $ aesonDrop 8 snakeCase


data ClubhouseWorkflowState = ClubhouseWorkflowState
  { chOldState :: Int
  , chNewState :: Int
  } deriving (Show, Generic)


instance FromJSON ClubhouseWorkflowState where
  parseJSON = withObject "thingy" $ \o -> do
    error (show o)

instance ToJSON ClubhouseWorkflowState where
  toJSON = genericToJSON $ aesonDrop 2 snakeCase


readEvent :: IO (Either String ClubhouseEvent)
readEvent = do
  let jsonData = C.readFile "clubhouse-event.json"
  events <- decodeChEvent <$> jsonData
  -- refs <- decodeChReferences <$> jsonData
  pure events


readReferences :: IO (Either String [IntMap Text])
readReferences = do
  let jsonData = C.readFile "clubhouse-event.json"
  decodeChReferences <$> jsonData


decodeChEvent :: ByteString -> Either String ClubhouseEvent
decodeChEvent = eitherDecodeStrict


decodeChReferences :: ByteString -> Either String [IntMap Text]
decodeChReferences bs = do
  refs <- eitherDecodeStrict bs
  pure $ mapper <$> filter predicate (chReferences refs)
  where
    predicate ref = case chReferenceEntityType ref of
         ClubhouseWorkflowEntity -> True
         _                       -> False
    mapper :: ClubhouseReference -> IntMap Text
    mapper ref = IntMap.fromList [(chReferenceId ref, chReferenceName ref)]


decodeChWorkflowState :: ByteString -> Either String ClubhouseWorkflowState
decodeChWorkflowState = eitherDecodeStrict