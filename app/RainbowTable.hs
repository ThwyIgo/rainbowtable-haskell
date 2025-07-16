{-# LANGUAGE OverloadedRecordDot #-}

module RainbowTable (genTable, lookup, genRandStrImpl, RainbowTable(..)) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.ByteString qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import System.Random
import Prelude hiding (lookup)

data RainbowTable = RainbowTable
  { table :: HashMap B.ByteString String,
    genChain :: String -> (B.ByteString, String)
  }

type PwSize = Int

type ChainSize = Int

type ChainCount = Int

-- Gera uma string aleatória de `charCount` caracteres da lista `charset`.
genRandStrImpl :: [Char] -> StdGen -> Int -> (String, StdGen)
genRandStrImpl charset randSeed charCount =
  let go :: Int -> StdGen -> String -> (String, StdGen)
      go 0 seed acc = (reverse acc, seed)
      go n seed acc =
        let (char, nextSeed) = randomCharFromCharset seed
         in go (n - 1) nextSeed (char : acc)
   in go charCount randSeed ""
  where
    randomCharFromCharset gen =
      let charsetLength = length charset
          (randomIndex, nextSeed) = randomR (0, charsetLength - 1) gen
       in (charset !! randomIndex, nextSeed)

-- Gera uma cadeia de aplicações de HashReduce. Retorna o último hash e a última senha
genChainImpl :: (String -> B.ByteString) -> (Integer -> B.ByteString -> String) -> ChainSize -> String -> (B.ByteString, String)
genChainImpl hash reduce chainSize initialPw =
      let hashReduce i = do
            currPw <- get
            let hashed = hash currPw
                reduced = reduce i hashed
            put reduced

          lastPw = flip execState initialPw $ sequence [hashReduce $ fromIntegral i | i <- [0 .. chainSize - 1]]
          lastHash = hash lastPw
       in (lastHash, lastPw)

-- Gera uma rainbowtable (HashMap)
genTableImpl ::
  ([Char] -> StdGen -> PwSize -> (String, StdGen)) ->
  ([String] -> [(B.ByteString, String)]) ->
  [Char] ->
  PwSize ->
  StdGen ->
  ChainCount ->
  (HashMap B.ByteString String, StdGen)
genTableImpl genRandStr genChains charset pwSize randSeed chainCount =
  let randStr' = do
        seed <- get
        let (str, nextSeed) = genRandStr charset seed pwSize
        put nextSeed
        return str
      (initialPws, nextSeed) = flip runState randSeed $ replicateM chainCount randStr'
      chains = genChains initialPws
  in (HashMap.fromList chains, nextSeed)

-- Gera uma rainbowtable imediatamente (Strict)
genTable ::
  ([Char] -> StdGen -> PwSize -> (String, StdGen)) ->
  ([Char] -> B.ByteString) ->
  ([Char] -> PwSize -> Integer -> B.ByteString -> String) ->
  [Char] ->
  PwSize ->
  ChainSize ->
  ChainCount ->
  StdGen ->
  (RainbowTable, StdGen)
genTable genRandStr hash reduce charset pwSize chainSize chainCount randSeed =
  let genChain = genChainImpl hash reduce' chainSize
      reduce' = reduce charset pwSize
      genChains = map (\s -> s <$ genChain s)
      !(!rt, nextSeed) = genTableImpl genRandStr genChains charset pwSize randSeed chainCount
   in ( RainbowTable
          { table = rt,
            genChain = genChain
          },
        nextSeed
      )

lookup :: RainbowTable -> B.ByteString -> Maybe String
lookup rt hashed = case HashMap.lookup hashed rt.table of
  Just initialPw -> Just $ snd $ rt.genChain initialPw
  a -> a