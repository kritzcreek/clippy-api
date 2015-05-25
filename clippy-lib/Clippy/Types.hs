{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Clippy.Types where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

data Yank = Yank
            { content      :: Text
            , timestamp    :: UTCTime
            , contentType :: Text
            } deriving (Show, Eq, Generic)

instance ToJSON Yank
instance FromJSON Yank

dummyYank :: UTCTime -> Yank
dummyYank x = Yank {
  content = "I'm a dummy Yank."
  , timestamp = x
  , contentType = "url"
  }
