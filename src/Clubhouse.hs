{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Clubhouse where

import           Control.Monad           (void)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Data.Aeson              (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import           Data.ByteString         (ByteString)
import           Data.Default.Class      (def)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           GHC.Generics            (Generic)
import           Network.Connection      (TLSSettings (TLSSettingsSimple),
                                          settingDisableCertificateValidation,
                                          settingDisableSession, settingUseServerName)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req        (GET (GET), HttpConfig, NoReqBody (NoReqBody),
                                          httpConfigAltManager, httpConfigCheckResponse, https,
                                          lbsResponse, req, responseBody, responseStatusCode,
                                          runReq, (/:), (/~), (=:))
import           Network.HTTP.Types      (statusCode)
import           Server                  (AppConfig (AppConfig), configClubhouseToken)
import qualified System.ReadEnvVar       as Env
import           URI.ByteString          (Absolute, URIRef, parseURI, strictURIParserOptions)


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


noTlsHttpConfig :: IO HttpConfig
noTlsHttpConfig = do
  manager <- noTlsManager
  pure $ httpConfig { httpConfigAltManager = Just manager}


httpConfig :: HttpConfig
httpConfig = def {httpConfigCheckResponse = check}


check :: p -> Client.Response a -> ByteString -> Maybe Client.HttpExceptionContent
check _ response preview =
  let scode = statusCode $ Client.responseStatus response
   in if (200 <= scode && scode < 300) || scode == 404
         then Nothing
       else Just (Client.StatusCodeException (void response) preview)


data Story = Story
  { storyType :: !Text
  , storyId   :: !Int
  , storyName :: !Text
  , storyUrl  :: !(URIRef Absolute)
  } deriving (Show, Generic)


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


test :: Int -> IO (Either StoryError Story)
test id_ = do
  tokenM <- Env.lookupEnv "CLUBHOUSE_API_TOKEN"
  case tokenM of
    Nothing -> error "Must set CLUBHOUSE_API_TOKEN"
    Just token -> do
      let appConf = AppConfig "we dont care about GH right now" token
      runReaderT (getStory id_) appConf


data StoryError
  = StoryParseError String
  | StoryNotFoundError
  deriving (Show)


getStory :: Int -> ReaderT AppConfig IO (Either StoryError Story)
getStory id_ = do
  config <- ask
  let chToken = configClubhouseToken config
  let url = https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ id_
  let conf = httpConfig
  runReq conf $ do
    r <- req GET url NoReqBody lbsResponse ("token" =: chToken)
    pure $
      if responseStatusCode r == 404
        then Left StoryNotFoundError
        else mapLeft StoryParseError (eitherDecode $ responseBody r)


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x
