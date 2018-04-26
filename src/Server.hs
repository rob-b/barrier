{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server
   where

import           Control.Concurrent.Async             (Async, async, wait)
import           Control.Concurrent.STM.TBMQueue      (TBMQueue, closeTBMQueue)
import           Control.Exception.Safe               (bracket)
import           Control.Logger.Simple                (LogConfig (LogConfig), withGlobalLogging)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.STM                    (atomically)
import           Data.Aeson                           (ToJSON, Value, object, (.=))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8
import           Data.HVect                           (HVect ((:&:), HNil))
import           Data.Text.Encoding                   (decodeUtf8)
import qualified Data.Vector                          as V
import           Events                               (selectEventType, selectResponse)
import           GitHub.Data.Webhooks.Secure          (isSecurePayload)
import           Network.HTTP.Types.Status            (Status (Status), status401, status422)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Queue                                as Q
import           System.Environment                   (lookupEnv)
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
  { appStateToken :: ByteString
  , appStateQueue :: TBMQueue (IO ())
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
  if isSecurePayload (decodeUtf8 $ appStateToken appState) signature payload
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
  event <- body
  eventKind <- rawHeader "X-Github-Event"
  let eventType = selectEventType =<< eventKind
  case selectResponse eventType event of
    Left reason -> do
      setStatus status422
      json reason
    Right value -> do
      queue <- appStateQueue <$> getState
      _ <- liftIO $ Q.add queue (void xo)
      json value


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
      key <- maybe mempty C8.pack <$> lookupEnv "GITHUB_KEY"
      let appState = AppState key queue
      spockCfg <- barrierConfig <$> defaultSpockCfg () PCNoDatabase appState
      runSpock 9000 (spock spockCfg $ middleware logger >> app)
    shutDown :: (TBMQueue a, Async b) -> IO b
    shutDown (queue, worker) = do
      atomically $ closeTBMQueue queue
      wait worker
