{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy.ES where

import           Clippy                    (hashYankHex)
import           Clippy.Types
import           Control.Applicative
import           Data.Aeson
import           Data.Either               (Either (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Data.Time.Calendar        (Day (..))
import           Data.Time.Clock           (UTCTime (..), getCurrentTime,
                                            secondsToDiffTime)
import           Database.Bloodhound
import           GHC.Generics              (Generic)
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Status as NHTS

testServer :: Server
testServer = Server "http://localhost:9200"

clippyIndex :: IndexName
clippyIndex = IndexName "clippy"

yankMapping :: MappingName
yankMapping = MappingName "yank"

withBH' :: BH IO a -> IO a
withBH' = withBH defaultManagerSettings testServer

defaultIndexSettings' :: IndexSettings
defaultIndexSettings' = IndexSettings (ShardCount 1) (ReplicaCount 1)

createIndex' :: IO Reply
createIndex' = withBH' $ createIndex defaultIndexSettings' clippyIndex

deleteIndex' :: IO Reply
deleteIndex' = withBH' $ deleteIndex clippyIndex

data YankMapping = YankMapping deriving (Show, Eq)

instance ToJSON YankMapping where
  toJSON YankMapping =
    object ["yank" .=
     object ["properties" .=
       object ["content_type" .=
         object ["type" .= ("string" :: Text)]]]]

createMapping' :: IO Reply
createMapping' = withBH' $ putMapping clippyIndex yankMapping YankMapping

deleteMapping' :: IO Reply
deleteMapping' = withBH' $ deleteMapping clippyIndex yankMapping

insertDummy :: IO Reply
insertDummy = do
  time <- getCurrentTime
  withBH' $ indexDocument
    clippyIndex
    yankMapping
    (dummyYank time)
    (DocId "1")

insertYank :: Yank -> BH IO Reply
insertYank y = indexDocument clippyIndex yankMapping y (DocId . hashYankHex $ y)

insertYank' :: Yank -> IO Reply
insertYank' = withBH' . insertYank

searchYank' :: SearchFilter -> IO [Yank]
searchYank' (SearchFilter amount q) =
  fmap (take amount . decodeSearch . responseBody) results
  where results = withBH' $ searchYank q
        extractHits = fmap hitSource . hits . searchHits
        decodeSearch resp = case eitherDecode resp of
          Right x -> extractHits x
          Left _ -> []

searchYank :: Text -> BH IO Reply
searchYank q = searchByIndex clippyIndex $ mkSearch (Just query) Nothing
  where query =  QueryMatchQuery $ mkMatchQuery (FieldName "content") (QueryString q)
