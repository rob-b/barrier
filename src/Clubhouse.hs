{-# LANGUAGE OverloadedStrings #-}

module Clubhouse (ask) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default.Class
import qualified Network.HTTP.Client    as Client
import           Network.HTTP.Req
import           Network.HTTP.Types     (statusCode)


httpConfig :: HttpConfig
httpConfig = def { httpConfigCheckResponse = check }


check _ response preview =
  let scode = statusCode' response
  in if (200 <= scode && scode < 300) || scode == 401
       then Nothing
       else Just (Client.StatusCodeException (void response) preview)

  where
    statusCode' :: Client.Response a -> Int
    statusCode' = statusCode . Client.responseStatus


ask :: Int -> IO ()
ask storyId =
  runReq httpConfig $ do
    r <-
      req
        GET
        (https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ storyId)
        NoReqBody
        jsonResponse
        ("token" =: ("something" :: String))
    let scode = responseStatusCode r
    liftIO $ print (responseBody r :: Value)
