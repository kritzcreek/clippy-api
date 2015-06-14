{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Clippy.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

data Yank = Yank
            { yankContent     :: Text
            , yankTimestamp   :: UTCTime
            , yankContentType :: Text
            } deriving (Show, Eq, Generic)

instance ToJSON Yank
instance FromJSON Yank

data YankSearchFilter = YankSearchFilter
                    { yankFilterAmount       :: Int
                    , yankFiltersearchString :: Text
                    } deriving (Show, Eq, Generic)

instance ToJSON YankSearchFilter
instance FromJSON YankSearchFilter where
  parseJSON (Object val) = YankSearchFilter <$>
                           val .:? "yankFilterAmount" .!= 10 <*>
                           val .:  "yankFilterSearchString"
  parseJSON _ = mzero

-- The set of currently supported languages

data Language =
  Haskell
  | JavaScript
  | Java
  | Scala
  | Ruby -- Just for you
  deriving (Eq, Show, Read, Generic)

instance ToJSON Language
instance FromJSON Language

data Snippet = Snippet
               {
                 snippetContent :: Text,
                 snippetLanguage       :: Language
               }
               deriving (Eq, Show, Generic)

instance ToJSON Snippet
instance FromJSON Snippet

data SnippetSearchFilter = SnippetSearchFilter
                           { snippetFilterAmount       :: Int,
                             snippetFilterLanguage     :: Language,
                             snippetFilterSearchString :: Text
                           } deriving (Show, Eq, Generic)

instance ToJSON SnippetSearchFilter
instance FromJSON SnippetSearchFilter where
  parseJSON (Object val) = SnippetSearchFilter <$>
                           val .:? "snippetFilterAmount" .!= 10 <*>
                           val .:  "snippetFilterLanguage" <*>
                           val .:  "snippetFilterSearchString"
  parseJSON _ = mzero


dummyYank :: UTCTime -> Yank
dummyYank x = Yank {
  yankContent = "I'm a dummy Yank."
  , yankTimestamp = x
  , yankContentType = "url"
  }
