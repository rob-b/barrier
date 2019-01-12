{-# LANGUAGE OverloadedStrings #-}
module Barrier.Check
  ( extractUrls
  , tokenise
  , filterByDomain
  , splitLines
  , extractClubhouseLinks
  ) where

import           Barrier.Clubhouse            (webappURIRefToApiUrl)
import           Barrier.Events.Types         (WrappedHook (WrappedHookIssueComment, WrappedHookPullRequest))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Either                  (rights)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           GitHub.Data.Webhooks.Payload (whIssueCommentBody, whPullReqBody)
import           Lens.Micro.Platform          ((^?), _Just)
import           URI.ByteString               (Absolute, URIParseError, URIRef, authorityHostL,
                                               authorityL, hostBSL, parseURI,
                                               strictURIParserOptions)


getHookBody :: WrappedHook -> Text
getHookBody hook
  | (WrappedHookIssueComment inner) <- hook = whIssueCommentBody inner
  | (WrappedHookPullRequest inner) <- hook = whPullReqBody inner
  | otherwise = ""


extractClubhouseLinks :: WrappedHook -> [Either URIParseError (URIRef Absolute)]
extractClubhouseLinks hook = filterByDomain (getHookBody hook) "app.clubhouse.io"


filterByDomain :: Text -> ByteString -> [Either URIParseError (URIRef Absolute)]
filterByDomain s domain =
  let predicate u = u ^? authorityL . _Just . authorityHostL . hostBSL == Just domain
  in [webappURIRefToApiUrl x | x <- extractUrls s, predicate x]


extractUrls :: Text -> [URIRef Absolute]
extractUrls t =
  let tokens = tokenise " " (encodeUtf8 $ splitLines t)
  in rights $ parseURI strictURIParserOptions <$> tokens


tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y =
  h :
  if B.null t
    then []
    else tokenise x (B.drop (B.length x) t)
  where
    (h, t) = B.breakSubstring x y


splitLines :: Text -> Text
splitLines txt = T.intercalate " " $ T.lines $ T.unlines $ T.splitOn "\r\n" txt
