{-# LANGUAGE OverloadedRecordDot #-}

module Storage (save, load) where

import Control.Monad (unless)
import Data.Binary (Binary (..), decodeFileOrFail, encodeFile)
import Data.ByteString qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import RainbowTable (RainbowTable (..), genChainImpl)
import System.Directory (doesFileExist)
import Prelude hiding (lookup)

-- newtype wrapper to avoid orphan instance
newtype SerializableHashMap k v = SerializableHashMap {unwrapHashMap :: HashMap k v}

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (SerializableHashMap k v) where
  put = put . HashMap.toList . unwrapHashMap
  get = SerializableHashMap . HashMap.fromList <$> get

-- | Saves the rainbow table to a file.
save :: FilePath -> RainbowTable -> IO ()
save path rt =
  encodeFile path (SerializableHashMap rt.table, rt.chainLength)

-- | Loads a rainbow table from a file.
load ::
  FilePath ->
  (String -> B.ByteString) ->
  ([Char] -> Int -> Integer -> B.ByteString -> String) ->
  [Char] ->
  Int ->
  IO RainbowTable
load path hash reduce charset pwSize = do
  fileExists <- doesFileExist path
  unless fileExists $ error "File does not exist"
  decoded <- decodeFileOrFail path
  case decoded of
    Left (_, err) -> error $ "Error decoding file: " ++ err
    Right (SerializableHashMap table, chainLength) ->
      let reduce' = reduce charset pwSize
          genChain = genChainImpl hash reduce' chainLength
       in return
            RainbowTable
              { table = table,
                genChain = genChain,
                chainLength = chainLength,
                hash = hash,
                reduce = reduce'
              }
