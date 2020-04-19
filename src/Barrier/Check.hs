{-# LANGUAGE OverloadedStrings #-}
module Barrier.Check
  ( extractUrls
  , tokenise
  , filterByDomain
  , splitLines
  , extractClubhouseLinks
  , extractClubhouseLinks2
  ) where

import           Barrier.Clubhouse            (webappURIRefToApiUrl)
import           Barrier.Clubhouse.Types      (ClubhouseLink)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Either                  (rights)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           GitHub.Data.Webhooks.Payload (HookPullRequest, whPullReqBody)
import           Lens.Micro.Platform          ((^?), _Just)
import           URI.ByteString
    (Absolute, URIRef, authorityHostL, authorityL, hostBSL, parseURI, strictURIParserOptions)


--------------------------------------------------------------------------------
extractClubhouseLinks :: HookPullRequest -> [ClubhouseLink]
extractClubhouseLinks hook = extractClubhouseLinks2 (whPullReqBody hook)


--------------------------------------------------------------------------------
extractClubhouseLinks2 :: Text -> [ClubhouseLink]
extractClubhouseLinks2 hook = filterByDomain "app.clubhouse.io" $ extractUrls hook


--------------------------------------------------------------------------------
filterByDomain :: ByteString -> [URIRef Absolute] -> [ClubhouseLink]
filterByDomain domain urls =
  let predicate u = u ^? authorityL . _Just . authorityHostL . hostBSL == Just domain
  in rights [webappURIRefToApiUrl x | x <- urls, predicate x]


--------------------------------------------------------------------------------
extractUrls :: Text -> [URIRef Absolute]
extractUrls t =
  let tokens = tokenise " " (encodeUtf8 $ splitLines t)
  in rights $ parseURI strictURIParserOptions <$> tokens


--------------------------------------------------------------------------------
tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y =
  h :
  if B.null t
    then []
    else tokenise x (B.drop (B.length x) t)
  where
    (h, t) = B.breakSubstring x y


--------------------------------------------------------------------------------
splitLines :: Text -> Text
splitLines txt = T.intercalate " " $ T.lines $ T.unlines $ T.splitOn "\r\n" txt
