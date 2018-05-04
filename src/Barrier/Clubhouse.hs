{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Barrier.Clubhouse where

import           Barrier.Server          (AppConfig, configClubhouseToken,
                                          mkAppConfig)
import           Control.Error           (handleExceptT, runExceptT)
import           Control.Exception       (SomeException)
import           Control.Monad           (void)
import           Control.Monad.Except    (ExceptT, mapExceptT)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Data.Aeson              (FromJSON, eitherDecode, parseJSON,
                                          withObject, (.:))
import           Data.ByteString         (ByteString)
import           Data.Default.Class      (def)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           GHC.Generics            (Generic)
import           Network.Connection      (TLSSettings (TLSSettingsSimple),
                                          settingDisableCertificateValidation,
                                          settingDisableSession,
                                          settingUseServerName)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req        (GET (GET), HttpConfig, LbsResponse,
                                          NoReqBody (NoReqBody),
                                          httpConfigAltManager,
                                          httpConfigCheckResponse, https,
                                          lbsResponse, req, responseBody,
                                          runReq, (/:), (/~), (=:))
import           Network.HTTP.Types      (statusCode)
import           URI.ByteString          (Absolute, URIRef, parseURI,
                                          strictURIParserOptions)


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


-- test :: Int -> IO (ExceptT StoryError IO Story)
test id_ = do
  appConfigM <- mkAppConfig
  case appConfigM of
    Nothing -> error "Must set CLUBHOUSE_API_TOKEN"
    Just appConfig -> do
      valueE <- runReaderT (getStory id_) appConfig
      value <- runExceptT valueE
      case value of
        Left reason  -> print $ "This failed because: " ++ show reason
        Right result -> print $ result


data StoryError
  = StoryParseError String
  | StoryNotFoundError
  deriving (Show)


getStory :: Int -> ReaderT AppConfig IO (ExceptT StoryError IO Story)
getStory id_ = do
  config <- ask
  let chToken = configClubhouseToken config
  let url = https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ id_
  let conf = httpConfig
  let response =
        handleExceptT handle (runReq conf $ req GET url NoReqBody lbsResponse ("token" =: chToken))
  pure $ mapExceptT yikes response
  where
    yikes :: IO (Either StoryError LbsResponse) -> IO (Either StoryError Story)
    yikes r = do
      xo <- r
      case xo of
        Left thing -> pure $ Left thing
        Right response' -> pure (mapLeft StoryParseError (eitherDecode $ responseBody response'))

    handle :: SomeException -> StoryError
    handle _ = StoryParseError "Make it up"


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x
