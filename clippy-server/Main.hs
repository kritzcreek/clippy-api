{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clippy              (findAllYanks, findYankById, insertNewYank, searchYanks)
import           Control.Monad.Trans
import           Data.Aeson          hiding (json)
import           Data.List           (find)
import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger

main :: IO ()
main =
  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $
      json $ object ["yank_url" .= String "/yanks"]
    get "/yanks" $ do
      contFilter <- fmap (find ((==) "contentType" . fst)) params
      json =<< liftIO (findAllYanks (fmap snd contFilter))
    get "/yanks/:yankId" $
      json =<< liftIO . findYankById =<< param "yankId"
    post "/yanks" $
      json =<< liftIO . insertNewYank =<< jsonData
    post "/yanks/search" $
      json =<< liftIO . searchYanks =<< jsonData
