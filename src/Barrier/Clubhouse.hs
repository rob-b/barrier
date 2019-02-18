{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Barrier.Clubhouse where

import           Barrier.Config          (AppConfig, configClubhouseToken, mkAppConfig, readish)
import           Control.Error           (runExceptT, throwE)
import           Control.Exception       (SomeException)
import           Control.Exception.Safe  (Exception, MonadCatch, fromException, tryJust)
import           Control.Logger.Simple   (logDebug)
import           Control.Monad           (void)
import           Control.Monad.Except    (ExceptT (ExceptT), mapExceptT)
import           Control.Monad.Reader    (MonadReader, ask, runReaderT)
import           Data.Aeson              (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C
import qualified Data.ByteString.Char8   as C8
import           Data.ByteString.Lazy    (toStrict)
import           Data.Default.Class      (def)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.Generics            (Generic)
import           Lens.Micro.Platform     ((^.))
import           Network.Connection      (TLSSettings (TLSSettingsSimple),
                                          settingDisableCertificateValidation,
                                          settingDisableSession, settingUseServerName)
import qualified Network.HTTP.Client     as Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req        (GET (GET), HttpConfig,
                                          HttpException (VanillaHttpException), LbsResponse,
                                          NoReqBody (NoReqBody), httpConfigAltManager,
                                          httpConfigCheckResponse, lbsResponse, parseUrlHttps, req,
                                          responseBody, responseStatusCode, runReq, (=:))
import           Network.HTTP.Types      (statusCode)
import           URI.ByteString          (Absolute, URIParseError (OtherError), URIRef, parseURI,
                                          pathL, serializeURIRef', strictURIParserOptions)


newtype ClubhouseLink = ClubhouseLink
  { unClubhouseLink :: URIRef Absolute
  } deriving (Show)


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
  let urlE = mkClubhouseStoryUrl id_
  case urlE of
    Left reason -> error $ show reason
    Right url -> do
      appConfigM <- mkAppConfig
      case appConfigM of
        Nothing -> error "Must set CLUBHOUSE_API_TOKEN"
        Just appConfig -> do
          valueE <- runReaderT (getStory url) appConfig
          value <- runExceptT valueE
          case value of
            Left StoryNotFoundError -> putStrLn "No matching story found."
            Left reason             -> putStrLn $ "This failed because: " <> show reason
            Right result            -> print result


data StoryError
  = StoryParseError String
  | StoryNotFoundError
  | StoryHttpError String
  | StoryInvalidLinkError String
  deriving (Show)


getStory :: MonadReader AppConfig m => ClubhouseLink -> m (ExceptT StoryError IO Story)
getStory (ClubhouseLink url) = do
  config <- ask
  case parseUrlHttps $ serializeURIRef' url of
    Nothing ->
      pure . throwE . StoryParseError $ "Could not parse url: " <> show (serializeURIRef' url)
    Just (url', option) -> do
      let chToken = decodeUtf8 $ configClubhouseToken config
      let option' = option <> ("token" =: chToken)
      let action = runReq httpConfig $ req GET url' NoReqBody lbsResponse option'
      let response = handleExceptT' handle action
      pure $ mapExceptT convertResponse response
  where
    convertResponse :: IO (Either StoryError LbsResponse) -> IO (Either StoryError Story)
    convertResponse responseE = do
      responseE >>= \case
        Left thing -> pure $ Left thing
        Right response' -> do
          if responseStatusCode response' == (404 :: Int)
            then pure $ Left StoryNotFoundError
            else logDebug (decodeUtf8 (toStrict (responseBody response'))) >>
                 pure (mapLeft StoryParseError (eitherDecode $ responseBody response'))


handleExceptT' :: (Exception e, Functor m, MonadCatch m) => (e -> Maybe x) -> m a -> ExceptT x m a
handleExceptT' handler = ExceptT . tryJust handler


handle :: SomeException -> Maybe StoryError
handle e =
  case fromException e of
    Just (VanillaHttpException _) -> pure . StoryHttpError $ show e
    _                             -> Nothing


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x


mkClubhouseStoryUrl :: Show a => a -> Either URIParseError ClubhouseLink
mkClubhouseStoryUrl storyID = ClubhouseLink <$> parseURI strictURIParserOptions (B.intercalate "" ["https://api.clubhouse.io/api/v2/stories/", C8.pack $ show storyID])


sampleUrl :: ByteString
sampleUrl = "https://app.clubhouse.io/zerodeposit/story/2944/create-contact-form-using-netlify"


webappUrlToApiUrl :: ByteString -> Either URIParseError ClubhouseLink
webappUrlToApiUrl url =
  case parseURI strictURIParserOptions url of
    Left reason -> Left reason
    Right url'  -> webappURIRefToApiUrl url'


webappURIRefToApiUrl :: URIRef Absolute -> Either URIParseError ClubhouseLink
webappURIRefToApiUrl url = response . getId $ C.split '/' (url ^. pathL)
  where
    getId (_:_:_:x:_) = readish (decodeUtf8 x)
    getId _           = Nothing
    response :: Maybe Integer -> Either URIParseError ClubhouseLink
    response Nothing    = Left $ OtherError ("Could not find story id in " <> show url)
    response (Just sid) = mkClubhouseStoryUrl sid
