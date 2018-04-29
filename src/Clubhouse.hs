{-# LANGUAGE OverloadedStrings #-}

module Clubhouse (ask) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default.Class
import           Network.HTTP.Req


httpConfig :: HttpConfig
httpConfig = def { httpConfigCheckResponse = \_ _ _ -> Nothing }

ask :: IO ()
ask =
  runReq httpConfig $ do
    let storyId = 123 :: Int
    r <-
      req
        GET
        (https "api.clubhouse.io" /: "api" /: "v2" /: "stories" /~ storyId)
        NoReqBody
        jsonResponse
        ("token" =: ("something" :: String))
    let scode = responseStatusCode r
    if (200 <= scode && scode < 300) || scode == 401
      then error "good"
      else error "bad"
    liftIO $ print (responseBody r :: Value)
