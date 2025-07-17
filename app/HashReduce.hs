module HashReduce (bs2s, s2bs, hash, reduce) where

import Crypto.Hash.SHA512 qualified as SHA512
import Data.Bits (Bits (shift))
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

bs2s :: B.ByteString -> String
bs2s = T.unpack . T.decodeUtf8 . B16.encode

s2bs :: String -> B.ByteString
s2bs str = case B16.decode (T.encodeUtf8 (T.pack str)) of
  Right s -> s
  _ -> error "String de entrada inválida: Caracteres hexadecimais inesperados"

hash :: String -> B.ByteString
hash str =
  let bytes = T.encodeUtf8 $ T.pack str
      hashed = SHA512.hash bytes
   in hashed

reduce :: [Char] -> Int -> Integer -> B.ByteString -> String
reduce charset pwLength roundIndex hashed =
    let
        hashValue :: Integer
        hashValue = B.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0 hashed

        lenCharset :: Integer
        lenCharset = fromIntegral (length charset)

        generateChar :: Int -> String -> String
        generateChar i acc
            | i >= pwLength = reverse acc
            | otherwise =
                let
                    idx :: Integer
                    idx = (hashValue `shift` negate (fromIntegral i * 8 + fromIntegral roundIndex)) `mod` lenCharset
                    char = charset !! fromIntegral idx
                in
                    generateChar (i + 1) (char : acc)
    in generateChar 0 ""