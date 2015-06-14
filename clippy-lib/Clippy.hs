{-# LANGUAGE OverloadedStrings #-}

module Clippy
(
  hashYank, hashYankHex, toHex
)where

import           Clippy.Types
import           Control.Applicative
import           Crypto.Hash.SHA256  (hash)
import qualified Data.ByteString     as BS
import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import           Data.Text.Encoding  (encodeUtf8)
import           Text.Printf         (printf)

toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"

hashYank :: Yank -> BS.ByteString
hashYank = hash . encodeUtf8 . ((<>) <$> content <*> contentType)

hashYankHex :: Yank -> Text
hashYankHex = pack . toHex . hashYank
