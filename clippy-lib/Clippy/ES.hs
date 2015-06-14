{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy.ES where

import           Clippy              (hashYankHex)
import           Clippy.Types
import           Data.Aeson          (eitherDecode)
import           Data.Text           (Text)
import           Data.Time.Clock     (getCurrentTime)
import           Database.Bloodhound
import           Network.HTTP.Client

testServer :: Server
testServer = Server "http://localhost:9200"

clippyIndex :: IndexName
clippyIndex = IndexName "clippy"

yankMapping :: MappingName
yankMapping = MappingName "yank"

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
  where query = QueryMatchQuery $ mkMatchQuery (FieldName "content") (QueryString q)

searchYank' :: SearchFilter -> IO [Yank]
searchYank' (SearchFilter amount q) =
  fmap (take amount . decodeSearch . responseBody) results
  where results = withBH' $ searchYank q
        extractHits = fmap hitSource . hits . searchHits
        decodeSearch resp = case eitherDecode resp of
          Right x -> extractHits x
          Left _ -> []
