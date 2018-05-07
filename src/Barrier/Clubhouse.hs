{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Barrier.Clubhouse where

import           Barrier.Config          (AppConfig, configClubhouseToken, mkAppConfig)
import           Control.Error           (handleExceptT, runExceptT)
import           Control.Exception       (SomeException)
import           Control.Monad           (void)
import           Control.Monad.Except    (ExceptT, mapExceptT)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Data.Aeson              (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import           Data.ByteString         (ByteString)
import           Data.Default.Class      (def)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.Generics            (Generic)
import           Network.Connection      (TLSSettings (TLSSettingsSimple),
                                          settingDisableCertificateValidation,
                                          settingDisableSession, settingUseServerName)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req        (GET (GET), HttpConfig, LbsResponse,
                                          NoReqBody (NoReqBody), httpConfigAltManager,
                                          httpConfigCheckResponse, https, lbsResponse, req,
                                          responseBody, responseStatusCode, runReq, (/:), (/~),
                                          (=:))
import           Network.HTTP.Types      (statusCode)
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


test :: Int -> IO ()
test id_ = do
  appConfigM <- mkAppConfig
  case appConfigM of
    Nothing -> error "Must set CLUBHOUSE_API_TOKEN"
    Just appConfig -> do
      valueE <- runReaderT (getStory id_) appConfig
      value <- runExceptT valueE
      case value of
        Left StoryNotFoundError -> putStrLn "No matching story found."
        Left reason             -> putStrLn $ "This failed because: " <> show reason
        Right result            -> print result


data StoryError
  = StoryParseError String
  | StoryNotFoundError
  | StoryHttpError String
  deriving (Show)


getStory :: Int -> ReaderT AppConfig IO (ExceptT StoryError IO Story)
getStory id_ = do
  config <- ask
  let chToken = decodeUtf8 $ configClubhouseToken config
  let url = https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ id_
  let conf = httpConfig
  let response =
        handleExceptT handle (runReq conf $ req GET url NoReqBody lbsResponse ("token" =: chToken))
  pure $ mapExceptT convertResponse response
  where
    convertResponse :: IO (Either StoryError LbsResponse) -> IO (Either StoryError Story)
    convertResponse responseE = do
      responseE >>= \case
        Left thing -> pure $ Left thing
        Right response' -> do
          if responseStatusCode response' == (404 :: Int)
            then pure $ Left StoryNotFoundError
            else pure (mapLeft StoryParseError (eitherDecode $ responseBody response'))

    handle :: SomeException -> StoryError
    handle e = StoryHttpError $ show e


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x
