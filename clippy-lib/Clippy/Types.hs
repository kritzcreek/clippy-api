{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric#-}
module Clippy.Types where

import Data.Time
import Data.Text
import Data.Aeson
import GHC.Generics

data Yank = Yank
            {
              content :: Text
            , timestamp :: UTCTime
            , content_type :: Text
            } deriving (Show, Eq, Generic)

instance ToJSON Yank
instance FromJSON Yank

dummyYank :: UTCTime -> Yank
dummyYank x = Yank {
  content = "I'm a dummy Yank."
  , timestamp = x
  , content_type = "url"
  }
