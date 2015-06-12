{-# LANGUAGE OverloadedStrings #-}

module Clippy
(
  mkYank, hashYank, hashYankHex,
  insertNewYank, findAllYanks,
  findYankById, searchYanks,
  toHex
)where

import           Clippy.Types
import           Control.Applicative
import           Crypto.Hash.SHA256    (hash)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8 (unpack)
import           Data.Maybe            (mapMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.Lazy        as L
import           Database.MongoDB      hiding (timestamp)
import           Prelude               hiding (lookup)
import           Text.Printf           (printf)

mkYank :: Document -> Maybe Yank
mkYank d = let doc = exclude ["_id"] d
           in Yank <$> lookup "content" doc
                   <*> lookup "timestamp" doc
                   <*> lookup "contentType" doc

toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"

toText :: BS.ByteString -> T.Text
toText = T.pack . BS8.unpack

hashYank :: Yank -> BS.ByteString
hashYank = hash . TE.encodeUtf8 . ((<>) <$> content <*> contentType)

hashYankHex :: Yank -> T.Text
hashYankHex = T.pack . toHex . hashYank

insertNewYank :: Yank -> IO T.Text
insertNewYank = insertYank

insertYank :: Yank -> IO T.Text
insertYank y = do
  pipe <- connect (host "127.0.0.1")
  yankId <- access pipe master "yanks" $
            insert "yanks" [ "content"     =: content y
                           , "timestamp"   =: timestamp y
                           , "contentType" =: contentType y ]
  close pipe
  return . T.pack . show $ yankId

findAllYanks :: Maybe L.Text -> IO [Yank]
findAllYanks typeFilter= do
  pipe <- connect (host "127.0.0.1")
  result <- access pipe master "yanks" $
    rest =<< find (select (maybe [] (
                              \x -> ["contentType" =: L.toStrict x])
                           typeFilter)
                   "yanks")
  close pipe
  return $ mapMaybe mkYank result

findYankById ::  String -> IO (Maybe Yank)
findYankById yankId = do
  print yankId
  pipe <- connect (host "127.0.0.1")
  result <- access pipe master "yanks" $
    findOne (select ["_id" := ObjId (read yankId)] "yanks")
  close pipe
  return $ mkYank =<< result

searchYanks :: SearchFilter -> IO [Yank]
searchYanks criteria = do
  pipe <- connect (host "127.0.0.1")
  results <- access pipe master "yanks" $
    find (select
          ["content" := RegEx (Regex (searchString criteria) "i")]
          "yanks") >>= nextN (amount criteria)
  close pipe
  return $ mapMaybe mkYank results
