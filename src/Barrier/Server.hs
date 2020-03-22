{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Barrier.Server
   where


import           Barrier.Clubhouse.Types
    ( ClubhouseAction
    , ClubhouseActionEntityType(ChaStory)
    , ClubhouseEvent(ClubhouseEvent)
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
import           Barrier.Config
    (AppConfig, configGitHubSecret, lookupEnv, mkAppConfig)
import           Barrier.Events
    (selectAction, selectEventType, selectResponse)
import qualified Barrier.Queue                        as Q
import           Control.Concurrent.Async             (Async, async, wait)
import           Control.Concurrent.STM.TBMQueue      (TBMQueue, closeTBMQueue)
import           Control.Exception.Safe               (bracket)
import           Control.Logger.Simple
    (LogConfig(LogConfig), logError, logInfo, withGlobalLogging)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.STM                    (atomically)
import           Data.Aeson                           (ToJSON, Value(Array), object, (.=))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C
import           Data.EitherR                         (EitherR(EitherR), runEitherR)
import           Data.HVect                           (HVect((:&:), HNil))
import qualified Data.IntMap                          as IntMap
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import qualified Data.Vector                          as V
import           Debug.Trace                          (trace, traceShow)
import           GitHub.Data.Webhooks.Secure          (isSecurePayload)
import           Network.HTTP.Types.Status            (Status(Status), status401, status422)
import           Network.Wai                          (Application, Middleware)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Spock
    ( ActionCtxT
    , SpockActionCtx
    , SpockM
    , body
    , get
    , getContext
    , getState
    , header
    , json
    , post
    , prehook
    , rawHeader
    , root
    , setStatus
    , spock
    , spockAsApp
    )
import           Web.Spock.Config
    (PoolOrConn(PCNoDatabase), SpockCfg, defaultSpockCfg, spc_errorHandler)


data SignedRequest = SignedRequest

data AppState = AppState
  { appStateQueue  :: TBMQueue (IO ())
  , appStateConfig :: AppConfig
  }


type Api = SpockM () () AppState ()
type ApiAction a = SpockActionCtx (HVect '[]) () () AppState a
type AuthedApiAction ctx a = SpockActionCtx ctx () () AppState a


--------------------------------------------------------------------------------
logger :: Middleware
logger = logStdoutDev


--------------------------------------------------------------------------------
initHook :: AuthedApiAction () (HVect '[])
initHook = return HNil


--------------------------------------------------------------------------------
authHook :: AuthedApiAction (HVect xs) (HVect (SignedRequest ': xs))
authHook = do
  oldCtx <- getContext
  appState <- getState
  payload <- body
  signature <- header "X-Hub-Signature"
  if isSecurePayload (decodeUtf8 (configGitHubSecret (appStateConfig appState))) signature payload
    then return (SignedRequest :&: oldCtx)
    else do
      setStatus status401
      json (errorObject (401 :: Int) "Invalid signature")


--------------------------------------------------------------------------------
errorObject :: ToJSON v => v -> ByteString -> Value
errorObject code msg =
  let inner = V.singleton $ object ["status" .= code, "detail" .= decodeUtf8 msg]
  in object ["errors" .= inner]


--------------------------------------------------------------------------------
errorHandler :: MonadIO m => Status -> ActionCtxT ctx m b
errorHandler (Status code msg) = json (errorObject code msg)


--------------------------------------------------------------------------------
barrierConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
barrierConfig cfg = cfg { spc_errorHandler = errorHandler }


--------------------------------------------------------------------------------
app :: Api
app = do
  prehook initHook $ do
    prehook authHook $
      post root handleEvent
    post "/ch" $ do
      handleCh
  get "/a" $ do
      queue <- appStateQueue <$> getState
      _ <- liftIO $ Q.add queue (void handleA)
      json ("{}" :: Value)


---------------------------------------------------------------------------------
handleA :: IO ()
handleA = do
  _ <- appendFile "example.txt" "this is my content\n"
  pure ()


---------------------------------------------------------------------------------
handleCh :: ApiAction a
handleCh = do
  bodyContent <- body
  case runEitherR (other bodyContent) of
    Left err -> do
      logError (T.pack err)
      setStatus status422
      json (errorObject (422 :: Int) (C.pack err))
    Right thing -> do
      logInfo (decodeUtf8 bodyContent)
      json thing


---------------------------------------------------------------------------------
other :: ByteString -> EitherR Value String
other bs = do
  failure <- EitherR $ convertBodyToEvent bs
  if bs == "null"
    then (EitherR (Right "Received \"null\" string"))
    else (pure failure)


--------------------------------------------------------------------------------
xo :: FilePath -> IO (Either String Value)
xo fname = do
  input <- B.readFile fname
  pure $ convertBodyToEvent input


--------------------------------------------------------------------------------
expandActions :: ByteString -> [ClubhouseAction] -> Either String Value
expandActions _ [] = Left "Event did not include a story change"
expandActions input actions = do
  -- case (eitherDecodeStrict input :: Either String ClubhouseReferences) of
  --   Left e     -> undefined
  --   Right refs -> trace (show refs) undefined
  case decodeChReferences input of
    Left err -> Left err
    Right (references :: IntMap.IntMap T.Text) -> do
      let objects = combiner references actions
      pure . Array $ V.fromList objects


combiner :: (Foldable t) => IntMap.IntMap T.Text -> t ClubhouseAction -> [Value]
combiner references actions =
  foldr (\action acc -> acc <> [defineActionChanges references action]) [] actions


--------------------------------------------------------------------------------
convertBodyToEvent :: ByteString -> Either String Value
convertBodyToEvent input =
  -- the input is the encoded json of a clubhouse event. Try and extract a clubhouse event from
  -- this json but only when at least one of its actions have the entity_type of "story"
  case mkChEventWithStories input of
    Left err -> Left err
    Right chEvent -> do
      output <- case chActions chEvent of
        -- we know that mkChEventWithStories should guarantee that the actions is not empty but
        -- that's not promised at the type level so we need to check still
        [] -> Left "Event did not include a story change"
        actions -> do
          case expandActions input actions of
            Left err -> do
              let reason = "Unable to parse references " ++ show err
              Left reason
            Right expandedActions -> do
              pure expandedActions
      pure output


--------------------------------------------------------------------------------
defineActionChanges :: IntMap.IntMap T.Text -> ClubhouseAction -> Value
defineActionChanges references action = do
  let na = traceShow action ("N/A" :: T.Text)
  case chActionWorkflowState action of
    (ClubhouseWorkflowCreated _workFlowId) -> undefined
    (ClubhouseWorkflowChanged state) -> do
      let newId = getWorkFlowId (chNewState $ state)
      let oldId = getWorkFlowId (chOldState $ state)
      object
        [ "title" .= fromMaybe na (chActionName action)
        , "id" .= chActionId action
        , "new_state" .= IntMap.lookup newId references
        , "old_state" .= IntMap.lookup oldId references
        ]


--------------------------------------------------------------------------------
mkChEventWithStories :: ByteString -> Either String ClubhouseEvent
mkChEventWithStories bs = do
  stories <- filter isChaStory . chActions <$> decodeChEvent bs
  if null stories
    then Left "Event has no actions where entity_type is \"story\""
    else Right $ ClubhouseEvent stories


--------------------------------------------------------------------------------
decodeChEventFilterStory :: ByteString -> Either String ClubhouseEvent
decodeChEventFilterStory bs = do
  ClubhouseEvent . filter isChaStory . chActions <$> decodeChEvent bs


--------------------------------------------------------------------------------
isChaStory :: ClubhouseAction -> Bool
isChaStory cha =
  case chActionEntityType cha of
    ChaStory -> True
    _        -> False


---------------------------------------------------------------------------------
handleEvent :: AuthedApiAction (HVect (SignedRequest ': xs)) a
handleEvent = do
  bs <- body
  eventHeader <- rawHeader "X-Github-Event"
  let eventType = flip selectEventType bs =<< eventHeader
  case eventType of
    Nothing -> do
      setStatus status422
      let reason = errorObject (422 :: Int) "Unsupported event"
      json reason
    Just wrappedEvent -> do
      case selectAction wrappedEvent of
        Nothing -> pure ()
        Just action -> do
          queue <- appStateQueue <$> getState
          config <- appStateConfig <$> getState
          _ <- trace "queing action" (liftIO $ Q.add queue (void $ action config))
          pure ()
      json (selectResponse wrappedEvent)


--------------------------------------------------------------------------------
run :: IO ()
run = withGlobalLogging (LogConfig Nothing True) (bracket setupApp shutdownApp runServer)
  where
    runServer :: (TBMQueue Q.Action, b) -> IO ()
    runServer (queue, _worker) = do
      appConfigM <- mkAppConfig
      case appConfigM of
        Nothing -> putStrLn "You must set GITHUB_KEY and CLUBHOUSE_API_TOKEN"
        Just appConfig -> do
          let appState = AppState queue appConfig
          (port, application) <- mkApp appState
          runApp port application


--------------------------------------------------------------------------------
shutdownApp :: (TBMQueue a, Async b) -> IO b
shutdownApp (queue, worker) = do
  atomically $ closeTBMQueue queue
  wait worker


--------------------------------------------------------------------------------
setupApp :: IO (TBMQueue Q.Action, Async ())
setupApp = do
  queue <- Q.make 100
  workerRef <- async $ Q.worker queue
  pure (queue, workerRef)


--------------------------------------------------------------------------------
mkApp :: AppState -> IO (Warp.Port, Application)
mkApp appState = do
  port <- maybe 9000  read <$> lookupEnv "PORT"
  spockConfig <- barrierConfig <$> defaultSpockCfg () PCNoDatabase appState
  app' <- spockAsApp (spock spockConfig app)
  pure (port, logger app')


--------------------------------------------------------------------------------
runApp :: Warp.Port -> Application -> IO ()
runApp port application = do
  let settings = Warp.setPort port Warp.defaultSettings
  Warp.runSettings settings application
