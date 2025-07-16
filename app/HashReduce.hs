module HashReduce (bs2s, s2bs, hash, reduce) where

import Crypto.Hash.SHA512 qualified as SHA512
import Crypto.Util (bs2i)
import Data.Bits ( Bits(shift, xor) )
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
reduce charset pwSize index hashed =
  let value = bs2i hashed
      mixed = value `xor` (0x5bd1e995 * index)
      lenCharset = fromIntegral $ length charset :: Integer
      l = [fromInteger $ mixed `shift` negate i `mod` lenCharset :: Int | i <- [0 .. pwSize - 1]]
      string = map (charset !!) l
   in string