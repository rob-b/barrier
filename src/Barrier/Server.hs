{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Barrier.Server
   where

import           Barrier.Config                       (AppConfig, configGitHubToken, mkAppConfig)
import           Barrier.Events                       (selectAction, selectEventType,
                                                       selectResponse)
import qualified Barrier.Queue                        as Q
import           Control.Concurrent.Async             (Async, async, wait)
import           Control.Concurrent.STM.TBMQueue      (TBMQueue, closeTBMQueue)
import           Control.Exception.Safe               (bracket)
import           Control.Logger.Simple                (LogConfig (LogConfig), withGlobalLogging)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.STM                    (atomically)
import           Data.Aeson                           (ToJSON, Value, object, (.=))
import           Data.ByteString                      (ByteString)
import           Data.HVect                           (HVect ((:&:), HNil))
import           Data.Text.Encoding                   (decodeUtf8)
import qualified Data.Vector                          as V
import           GitHub.Data.Webhooks.Secure          (isSecurePayload)
import           Network.HTTP.Types.Status            (Status (Status), status401, status422)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Spock                            (ActionCtxT, SpockActionCtx, SpockM, body,
                                                       get, getContext, getState, header, json,
                                                       middleware, post, prehook, rawHeader, root,
                                                       runSpock, setStatus, spock)
import           Web.Spock.Config                     (PoolOrConn (PCNoDatabase), SpockCfg,
                                                       defaultSpockCfg, spc_errorHandler)


logger :: Middleware
logger = logStdoutDev

data SignedRequest = SignedRequest

data AppState = AppState
  { appStateQueue  :: TBMQueue (IO ())
  , appStateConfig :: AppConfig
  }


type Api = SpockM () () AppState ()
type ApiAction a = SpockActionCtx (HVect '[]) () () AppState a
type AuthedApiAction ctx a = SpockActionCtx ctx () () AppState a


initHook :: AuthedApiAction () (HVect '[])
initHook = return HNil


authHook :: AuthedApiAction (HVect xs) (HVect (SignedRequest ': xs))
authHook = do
  oldCtx <- getContext
  appState <- getState
  payload <- body
  signature <- header "X-Hub-Signature"
  if isSecurePayload (decodeUtf8 (configGitHubToken (appStateConfig appState))) signature payload
    then return (SignedRequest :&: oldCtx)
    else do
      setStatus status401
      json (errorObject (401 :: Int) "Invalid signature")


errorObject :: ToJSON v => v -> ByteString -> Value
errorObject code msg =
  let inner = V.singleton $ object ["status" .= code, "detail" .= decodeUtf8 msg]
  in object ["errors" .= inner]


errorHandler :: MonadIO m => Status -> ActionCtxT ctx m b
errorHandler (Status code msg) = json (errorObject code msg)


barrierConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
barrierConfig cfg = cfg { spc_errorHandler = errorHandler }


app :: Api
app = do
  prehook initHook $ do
    prehook authHook $
      post root handleEvent
  get "/a" $ do
      queue <- appStateQueue <$> getState
      _ <- liftIO $ Q.add queue (void xo)
      json ("{}" :: Value)


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
    Just wrapped -> do
      let actionM = selectAction wrapped
      case actionM of
        Nothing -> pure ()
        Just action -> do
          queue <- appStateQueue <$> getState
          config <- appStateConfig <$> getState
          _ <- liftIO $ Q.add queue (void $ action config)
          pure ()
      json (selectResponse wrapped)


xo :: IO ()
xo = do
  _ <- appendFile "example.txt" "this is my content\n"
  pure ()


run :: IO ()
run = withGlobalLogging (LogConfig (Just "logfile.txt") False) (bracket setup shutDown runServer)
  where
    setup :: IO (TBMQueue Q.Action, Async ())
    setup = do
      queue <- Q.make 100
      workerRef <- async $ Q.worker queue
      pure (queue, workerRef)
    runServer :: (TBMQueue Q.Action, b) -> IO ()
    runServer (queue, _worker) = do
      appConfigM <- mkAppConfig
      case appConfigM of
        Nothing -> putStrLn "You must set GITHUB_KEY and CLUBHOUSE_API_TOKEN"
        Just appConfig -> do
          let appState = AppState queue appConfig
          spockCfg <- barrierConfig <$> defaultSpockCfg () PCNoDatabase appState
          runSpock 9000 (spock spockCfg $ middleware logger >> app)
    shutDown :: (TBMQueue a, Async b) -> IO b
    shutDown (queue, worker) = do
      atomically $ closeTBMQueue queue
      wait worker
