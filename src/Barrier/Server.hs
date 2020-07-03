{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Barrier.Server
  ( AppState(AppState)
  , mkApp
  , setupApp
  , shutdownApp
  , run
  , queue
  ) where

import           Barrier.Config
    (AppConfig, configGitHubSecret, configPort, mkAppConfig)
import           Barrier.Handlers.Clubhouse           (processClubhouseWebhook)
import           Barrier.Handlers.GitHub
    ( EventBody(EventBody)
    , EventHeader(EventHeader)
    , UnsupportedEvent(UnsupportedEvent)
    , selectAction
    , selectEventType
    , selectResponse
    )
import qualified Barrier.Queue                        as Q
import           Control.Concurrent.Async             (Async, async, wait)
import           Control.Concurrent.STM.TBMQueue      (TBMQueue, closeTBMQueue)
import           Control.Exception.Safe               (bracket)
import           Control.Logger.Simple
    (LogConfig(LogConfig), logError, logInfo, withGlobalLogging)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader(ask), runReaderT)
import           Control.Monad.Reader.Class           (asks)
import           Control.Monad.STM                    (atomically)
import           Data.Aeson                           (ToJSON, Value, encode, object, (.=))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C
import           Data.ByteString.Lazy                 (toStrict)
import           Data.HVect                           (HVect((:&:), HNil))
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import qualified Data.Vector                          as V
import           Debug.Trace                          (trace)
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
routes :: Api
routes = do
  prehook initHook $ do
    prehook authHook $
      post root handleGithub
    post "/ch" $ do
      handleClubhouse
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
handleClubhouse :: ApiAction a
handleClubhouse = do
  bodyContent <- body
  case processClubhouseWebhook bodyContent of
    Left err -> do
      logError (T.pack $ show err)
      setStatus status422
      json (errorObject (422 :: Int) (C.pack $ show err))
    Right expandedActions -> do
      logInfo . decodeUtf8 . toStrict $ encode expandedActions
      json expandedActions


---------------------------------------------------------------------------------
handleGithub :: AuthedApiAction (HVect (SignedRequest ': xs)) a
handleGithub = do
  bs <- body
  (eventHeader :: Maybe ByteString) <- rawHeader "X-Github-Event"
  case eventHeader of
    Nothing -> do
      setStatus status422
      let reason = errorObject (422 :: Int) "Missing event header."
      logInfo "Unsupported event"
      json reason
    Just eventHeader' -> do
      case selectEventType (EventHeader eventHeader') (EventBody bs) of
        Left (UnsupportedEvent err) -> do
          setStatus status422
          let msg = "Unsupported event: " <> err
          let reason = errorObject (422 :: Int) msg
          logInfo $ decodeUtf8 msg
          json reason
        Right wrappedEvent -> do
          let action = selectAction wrappedEvent
          logInfo . decodeUtf8 $ "X-Github-Event: " <> eventHeader'
          queue <- appStateQueue <$> getState
          config <- appStateConfig <$> getState
          _ <- trace "queing action" (liftIO $ Q.add queue (void $ action config))
          json (selectResponse wrappedEvent)


--------------------------------------------------------------------------------
run :: IO ()
run =
  withGlobalLogging
    (LogConfig Nothing True)
    (bracket setupApp (runReaderT shutdownApp) (runReaderT runServer))


--------------------------------------------------------------------------------
runServer :: (MonadIO m, MonadReader Env m) => m ()
runServer = do
  appConfigM <- asks config
  case appConfigM of
    Nothing -> liftIO $ putStrLn "You must set GITHUB_KEY and CLUBHOUSE_API_TOKEN"
    Just appConfig -> do
      env <- ask
      let appState = AppState (queue env) appConfig
      let port = configPort appConfig
      application <- liftIO $ mkApp appState
      _ <- logInfo . T.pack $ "Starting app on port " <> show port
      liftIO $ Warp.run port application


--------------------------------------------------------------------------------
shutdownApp :: (MonadIO m, MonadReader Env m) => m ()
shutdownApp = do
  env <- ask
  liftIO $ atomically $ closeTBMQueue (queue env)
  liftIO $ wait (worker env)


--------------------------------------------------------------------------------
setupApp :: IO Env
setupApp = do
  appConfigM <- mkAppConfig
  q <- Q.make 100
  let worker' = Q.worker q
  workerRef <- async worker'
  pure Env {queue = q, worker = workerRef, config = appConfigM}


--------------------------------------------------------------------------------
data Env = Env
  { queue  :: TBMQueue Q.Action
  , worker :: Async ()
  , config :: Maybe AppConfig
  }


--------------------------------------------------------------------------------
mkApp :: AppState -> IO (Application)
mkApp appState = do
  spockConfig <- barrierConfig <$> defaultSpockCfg () PCNoDatabase appState
  app' <- spockAsApp (spock spockConfig routes)
  pure $ logger app'
