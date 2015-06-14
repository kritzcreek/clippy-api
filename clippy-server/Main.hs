{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clippy.ES                            (insertYank', searchYank')
import           Control.Monad.Trans                  (liftIO)
import           Data.Aeson                           hiding (json)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

main :: IO ()
main =
  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $
      json $ object ["yank_url" .= String "/yanks"]
    post "/yanks" $
      json . show =<< liftIO . insertYank' =<< jsonData
    post "/yanks/search" $
      json =<< liftIO . searchYank' =<< jsonData
