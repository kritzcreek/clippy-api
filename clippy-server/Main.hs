{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.Time
import Clippy (insertNewYank, toHex)
import Clippy.Types (dummyYank)
import Data.Monoid (mconcat)
import Database.HDBC
import Database.HDBC.Sqlite3

main :: IO ()
main = do
  conn <- connectSqlite3 "test1.db"
  scotty 3000 $ do
    get "/" $ do
      json $ object ["yank_url" .= String "/yanks"]
    get "/yanks" $ do
      time <- liftIO getCurrentTime
      json [dummyYank time]
    post "/yanks" $
      json =<< liftIO . fmap toHex . insertNewYank =<< jsonData
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
