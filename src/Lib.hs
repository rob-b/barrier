{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
   where

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8
import           Data.HVect                           (HVect ((:&:), HNil))
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import           Data.Text.Encoding                   (decodeUtf8)
import           GitHub.Data.Webhooks.Secure          (isSecurePayload)
import           Network.HTTP.Types.Status            (status201, status401, status422)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)
import           Web.Spock                            (SpockActionCtx, SpockM, body, get,
                                                       getContext, getState, header, middleware,
                                                       prehook, root, runSpock, setStatus, spock,
                                                       text)
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
  prehook authHook $ get root (text "yeah?")


initHook :: AuthedApiAction () (HVect '[])
initHook = return HNil


authCheck :: Maybe Text -> ByteString -> Bool
authCheck Nothing _  = False
authCheck (Just x) y = x == "Bearer " <> decodeUtf8 y


authHook :: AuthedApiAction (HVect xs) (HVect (User ': xs))
authHook = do
  oldCtx <- getContext
  appState <- getState
  payload <- body
  signature <- header "X-Hub-Signature"
  if isSecurePayload (decodeUtf8 $ appStateToken appState) signature payload
    then return (User :&: oldCtx)
    else do
      setStatus status401
      text "get lost"


-- barrierConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
-- barrierConfig cfg = cfg { spc_errorHandler = errorHandler }


run :: IO ()
run = do
  key <- maybe mempty C8.pack <$> lookupEnv "GITHUB_KEY"
  let appState = AppState key
  spockCfg <- defaultSpockCfg () PCNoDatabase appState
  runSpock 8000 (spock spockCfg $ middleware logger >> app)
