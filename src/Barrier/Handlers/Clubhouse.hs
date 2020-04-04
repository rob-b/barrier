{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Barrier.Handlers.Clubhouse
   where


import           Barrier.Clubhouse.Types
    ( ClubhouseAction
    , ClubhouseActionEntityType(ChaStory)
    , ClubhouseEvent(ClubhouseEvent)
    , EventParseError(EventParseError)
    , ExpandedAction(ExpandedAction)
    , WorkflowStateOptions(ClubhouseWorkflowChanged, ClubhouseWorkflowCreated)
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
    )
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.EitherR            (EitherR(EitherR), runEitherR)
import qualified Data.IntMap             as IntMap
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           Debug.Trace             (traceShow)


---------------------------------------------------------------------------------
processClubhouseWebhook :: ByteString -> Either EventParseError (Vector ExpandedAction)
processClubhouseWebhook bs = runEitherR $ do
  failure <- EitherR $ convertBodyToEvent bs
  case bs of
    "null" -> EitherR (Left $ EventParseError "Received \"null\" string!")
    ""     -> EitherR (Left $ EventParseError "Received empty body")
    _      -> pure failure


--------------------------------------------------------------------------------
convertBodyToEvent :: ByteString -> Either EventParseError (Vector ExpandedAction)
convertBodyToEvent input =
  -- the input is the encoded json of a clubhouse event. Try and extract a clubhouse event from
  -- this json but only when at least one of its actions have the entity_type of "story"
  case mkChEventWithStories input of
    Left err -> Left err
    Right chEvent -> do
      output <- case chActions chEvent of
        -- we know that mkChEventWithStories should guarantee that the actions is not empty but
        -- that's not promised at the type level so we need to check still
        [] -> Left $ EventParseError "Event did not include a story change"
        (actions :: [ClubhouseAction]) -> do
          case expandActions input actions of
            Left err -> do
              let reason = "Unable to parse references " ++ show err
              Left $ EventParseError reason
            Right expandedActions -> do
              pure expandedActions
      pure output


--------------------------------------------------------------------------------
-- Given a bytestring representing the webhook body try and extract any stories
mkChEventWithStories :: ByteString -> Either EventParseError ClubhouseEvent
mkChEventWithStories bs = do
  stories <- filter isChaStory . chActions <$> decodeChEvent bs
  if null stories
    then Left $ EventParseError "Event has no actions where entity_type is \"story\""
    else Right $ ClubhouseEvent stories


--------------------------------------------------------------------------------
expandActions :: ByteString -> [ClubhouseAction] -> Either String (Vector ExpandedAction)
expandActions _ [] = Left "Event did not include a story change"
expandActions input actions = do
  case decodeChReferences input of
    Left err -> Left err
    Right (references :: IntMap.IntMap T.Text) -> do
      let objects = combiner references actions
      pure $ V.fromList objects


--------------------------------------------------------------------------------
combiner :: (Foldable t) => IntMap.IntMap T.Text -> t ClubhouseAction -> [ExpandedAction]
combiner references actions =
  foldr (\action acc -> acc <> [defineActionChanges references action]) [] actions


--------------------------------------------------------------------------------
-- Does the given clubhouse action represent a story?
isChaStory :: ClubhouseAction -> Bool
isChaStory cha =
  case chActionEntityType cha of
    ChaStory -> True
    _        -> False


--------------------------------------------------------------------------------
defineActionChanges :: IntMap.IntMap T.Text -> ClubhouseAction -> ExpandedAction
defineActionChanges references action = do
  let na = traceShow action ("N/A" :: T.Text)
  case chActionWorkflowState action of
    (ClubhouseWorkflowCreated _workFlowId) -> undefined
    (ClubhouseWorkflowChanged state) -> do
      let newId = getWorkFlowId (chNewState state)
      let oldId = getWorkFlowId (chOldState state)
      let title = fromMaybe na (chActionName action)
      ExpandedAction
        title
        (chActionId action)
        (IntMap.lookup newId references)
        (IntMap.lookup oldId references)


--------------------------------------------------------------------------------
-- Helper function for converting a locally stored version of a clubhouse message
xo :: FilePath -> IO (Either EventParseError (Vector ExpandedAction))
xo fname = do
  convertBodyToEvent <$> B.readFile fname
