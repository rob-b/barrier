{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server
   where

import           Control.Logger.Simple                (LogConfig (LogConfig), withGlobalLogging)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Aeson                           (Value, object, (.=))
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
import           System.Environment                   (lookupEnv)
import           Web.Spock                            (ActionCtxT, SpockActionCtx, SpockM, body,
                                                       get, getContext, getState, header, json,
                                                       middleware, post, prehook, rawHeader, root,
                                                       runSpock, setStatus, spock, text)
import           Web.Spock.Config                     (PoolOrConn (PCNoDatabase), SpockCfg,
                                                       defaultSpockCfg, spc_errorHandler)


logger :: Middleware
logger = logStdoutDev

data SignedRequest = SignedRequest

newtype AppState = AppState
  { appStateToken :: ByteString
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
      text "get lost"


errorHandler :: MonadIO m => Status -> ActionCtxT ctx m b
errorHandler = json . prepareError
  where
    prepareError :: Status -> Value
    prepareError (Status code msg) =
      let inner = V.singleton $ object ["status" .= code, "detail" .= decodeUtf8 msg]
      in object ["errors" .= inner]


barrierConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
barrierConfig cfg = cfg { spc_errorHandler = errorHandler }


app :: Api
app = do
  prehook initHook $ do
    prehook authHook $
      post root handleEvent
  get "/a" $ do text "rocking it"


handleEvent :: AuthedApiAction (HVect (SignedRequest ': xs)) a
handleEvent = do
  event <- body
  eventKind <- rawHeader "X-Github-Event"
  let eventType = selectEventType =<< eventKind
  case selectResponse eventType event of
    Left reason -> do
      setStatus status422
      json reason
    Right value -> json value


run :: IO ()
run =
  withGlobalLogging (LogConfig (Just "logfile.txt") False) $ do
    key <- maybe mempty C8.pack <$> lookupEnv "GITHUB_KEY"
    let appState = AppState key
    spockCfg <- barrierConfig <$> defaultSpockCfg () PCNoDatabase appState
    runSpock 9000 (spock spockCfg $ middleware logger >> app)
