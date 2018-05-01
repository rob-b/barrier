{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Clubhouse where

import           Control.Exception.Safe  (Exception, MonadCatch, try)
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON, decode, parseJSON, withObject, (.:))
import           Data.ByteString         (ByteString)
import           Data.Default.Class      (def)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import           Debug.Trace
import           GHC.Generics            (Generic)
import           Network.Connection      (TLSSettings (..))
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req        (GET (GET), HttpConfig, NoReqBody (NoReqBody),
                                          httpConfigAltManager, httpConfigCheckResponse, https,
                                          jsonResponse, lbsResponse, req, responseBody,
                                          responseStatusCode, runReq, (/:), (/~), (=:))
import           Network.HTTP.Types      (statusCode)
import qualified System.ReadEnvVar       as Env


noVerifyTlsManagerSettings :: Client.ManagerSettings
noVerifyTlsManagerSettings =
  mkManagerSettings
    TLSSettingsSimple
    { settingDisableCertificateValidation = True
    , settingDisableSession = True
    , settingUseServerName = True
    }
    Nothing


noTlsManager :: IO Client.Manager
noTlsManager = Client.newManager noVerifyTlsManagerSettings


httpConfig :: IO HttpConfig
httpConfig = do
  manager <- noTlsManager
  pure $ def { httpConfigCheckResponse = check, httpConfigAltManager = Just manager}


check :: p -> Client.Response a -> ByteString -> Maybe Client.HttpExceptionContent
check _ response preview =
  let scode = statusCode $ Client.responseStatus response
   in if (200 <= scode && scode < 300) || scode == 404
         then Nothing
       else Just (Client.StatusCodeException (void response) preview)


data Story = Story
  { storyType :: Text
  , storyId   :: Int
  , storyName :: Text
  } deriving (Show, Generic)


instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> do
    storyType <- o .: "story_type"
    storyId <- o .: "id"
    storyName <- o .: "name"
    pure Story {..}


ask
  :: (MonadIO m, Exception e, MonadCatch m)
  => Int -> m (Either e (Maybe Story))
ask = try . test


test
  :: (MonadIO m)
  => Int -> m (Maybe Story)
test id_ = do
  token <- Env.lookupEnv "CLUBHOUSE_API_TOKEN"
  let token' = fromMaybe "invalid" token
  conf <- liftIO httpConfig
  runReq conf $ do
    r <-
      req
        GET
        (https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ id_)
        NoReqBody
        lbsResponse
        ("token" =: (token' :: String))
    pure $
      if responseStatusCode r == 404
        then Nothing
        else traceShow (responseBody r) (decode $ responseBody r)
