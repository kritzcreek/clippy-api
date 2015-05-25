module Clippy where

import qualified Data.ByteString as BS
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as L
import Crypto.Hash.SHA256 (hashlazy)
import Clippy.Types
import Text.Printf (printf)

toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"

hashYank :: Yank -> BS.ByteString
hashYank = hashlazy . T.encodeUtf8 . L.fromStrict . content

insertNewYank :: Yank -> IO BS.ByteString
insertNewYank = return . hashYank
