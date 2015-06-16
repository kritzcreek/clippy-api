{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy.ES where

import           Clippy              (hashYankHex, hashSnippetHex)
import           Clippy.Types
import           Data.Aeson          (eitherDecode)
import           Data.Text           (Text, pack)
import           Data.Time.Clock     (getCurrentTime)
import           Database.Bloodhound
import           Network.HTTP.Client

testServer :: Server
testServer = Server "http://localhost:9200"

clippyIndex :: IndexName
clippyIndex = IndexName "clippy"

yankMapping :: MappingName
yankMapping = MappingName "yank"

snippetMapping :: MappingName
snippetMapping = MappingName "snippet"

withBH' :: BH IO a -> IO a
withBH' = withBH defaultManagerSettings testServer

insertDummy :: IO Reply
insertDummy = insertYank' . dummyYank =<< getCurrentTime

insertYank :: Yank -> BH IO Reply
insertYank y = indexDocument clippyIndex yankMapping y (DocId . hashYankHex $ y)

insertYank' :: Yank -> IO Reply
insertYank' = withBH' . insertYank

searchYank :: Text -> BH IO Reply
searchYank q = searchByIndex clippyIndex $ mkSearch (Just query) Nothing
  where query = QueryMatchQuery $ mkMatchQuery (FieldName "yankContent") (QueryString q)

searchYank' :: YankSearchFilter -> IO [Yank]
searchYank' (YankSearchFilter amount q) =
  fmap (take amount . decodeSearch . responseBody) results
  where results = withBH' $ searchYank q
        extractHits = fmap hitSource . hits . searchHits
        decodeSearch resp = case eitherDecode resp of
          Right x -> extractHits x
          Left _ -> []

insertSnippet :: Snippet -> BH IO Reply
insertSnippet s = indexDocument clippyIndex snippetMapping s (DocId . hashSnippetHex $ s)

insertSnippet' :: Snippet -> IO Reply
insertSnippet' = withBH' . insertSnippet

searchSnippet :: Language -> Text -> BH IO Reply
searchSnippet language q = searchByIndex clippyIndex $ mkSearch (Just query) (Just filter')
  where query = QueryMatchQuery $ mkMatchQuery (FieldName "snippetContent") (QueryString q)
        filter' = BoolFilter $ MustMatch (Term "snippetLanguage" (pack (show language))) False

searchSnippet' :: SnippetSearchFilter -> IO [Snippet]
searchSnippet' (SnippetSearchFilter amount language q) =
  fmap (take amount . decodeSearch . responseBody) results
  where results = withBH' $ searchSnippet language q
        extractHits = fmap hitSource . hits . searchHits
        decodeSearch resp = case eitherDecode resp of
          Right x -> extractHits x
          Left _ -> []
