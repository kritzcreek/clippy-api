{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Clippy.Types where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Control.Applicative
import Control.Monad

data Yank = Yank
            { content      :: Text
            , timestamp    :: UTCTime
            , contentType :: Text
            } deriving (Show, Eq, Generic)

instance ToJSON Yank
instance FromJSON Yank

data SearchFilter = SearchFilter
                    {
                      amount :: Int
                    , searchString :: Text
                    } deriving (Show, Eq, Generic)

instance ToJSON SearchFilter
instance FromJSON SearchFilter where
  parseJSON (Object val) = SearchFilter <$>
                           val .:? "amount" .!= 10 <*>
                           val .:  "searchString"
  parseJSON _ = mzero

dummyYank :: UTCTime -> Yank
dummyYank x = Yank {
  content = "I'm a dummy Yank."
  , timestamp = x
  , contentType = "url"
  }
