{-# LANGUAGE OverloadedStrings #-}
module Barrier.Check
  ( extractUrls
  , tokenise
  , filterByDomain
  ) where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import           Data.Either        (rights)
import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)
import           URI.ByteString     (Absolute, URIRef, authorityHost, hostBS, parseURI,
                                     strictURIParserOptions, uriAuthority)


filterByDomain :: Text -> ByteString -> [URIRef Absolute]
filterByDomain s domain =
  let urls = extractUrls s
      predicate u = (hostBS <$> (authorityHost <$> uriAuthority u)) == Just domain
  in [x | x <- urls, predicate x]


extractUrls :: Text -> [URIRef Absolute]
extractUrls t =
  let tokens = tokenise " " (encodeUtf8 t)
  in rights $ parseURI strictURIParserOptions <$> tokens


tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y =
  h :
  if B.null t
    then []
    else tokenise x (B.drop (B.length x) t)
  where
    (h, t) = B.breakSubstring x y
