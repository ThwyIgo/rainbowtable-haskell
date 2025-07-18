{-# LANGUAGE OverloadedRecordDot #-}

module RainbowTable (genTable, lookup, genRandStrImpl, genChainImpl, RainbowTable (..)) where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.ByteString qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (isJust)
import System.Random
import Prelude hiding (lookup)

data RainbowTable = RainbowTable
  { table :: HashMap B.ByteString String,
    genChain :: String -> [(B.ByteString, String)],
    chainLength :: ChainLength,
    hash :: String -> B.ByteString,
    reduce :: Integer -> B.ByteString -> String
  }

type PwLength = Int

type ChainLength = Int

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

-- Gera uma cadeia de aplicações de HashReduce. Retorna pares de hash e senha na ordem da cadeia.
genChainImpl :: (String -> B.ByteString) -> (Integer -> B.ByteString -> String) -> ChainLength -> String -> [(B.ByteString, String)]
genChainImpl hash reduce chainSize initialPw =
  let hashReduce i = do
        currPw <- get
        let hashed = hash currPw
            reduced = reduce i hashed
        put reduced
        return (hashed, currPw)
   in flip evalState initialPw $ sequence [hashReduce $ fromIntegral i | i <- [1 .. chainSize]]

-- Gera uma rainbowtable (HashMap)
genTableImpl ::
  ([Char] -> StdGen -> PwLength -> (String, StdGen)) ->
  ([String] -> [(B.ByteString, String)]) ->
  [Char] ->
  PwLength ->
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
  ([Char] -> StdGen -> PwLength -> (String, StdGen)) ->
  (String -> B.ByteString) ->
  ([Char] -> PwLength -> Integer -> B.ByteString -> String) ->
  [Char] ->
  PwLength ->
  ChainLength ->
  ChainCount ->
  StdGen ->
  (RainbowTable, StdGen)
genTable genRandStr hash reduce charset pwSize chainSize chainCount randSeed =
  let genChain = genChainImpl hash reduce' chainSize
      reduce' = reduce charset pwSize
      genChains l = map (\s -> s <$ last (genChain s)) l `using` parListChunk 100 rdeepseq
      !(!rt, nextSeed) = genTableImpl genRandStr genChains charset pwSize randSeed chainCount
   in ( RainbowTable
          { table = rt,
            genChain = genChain,
            chainLength = chainSize,
            hash = hash,
            reduce = reduce'
          },
        nextSeed
      )

lookup :: RainbowTable -> B.ByteString -> Maybe String
lookup rt hashed =
  let initialPw = lookupInit rt hashed
   in case initialPw of
        Just pw -> msum (findHash hashed . rt.genChain <$> pw)
        Nothing -> Nothing
  where
    findHash _ [] = Nothing
    findHash target ((h, pw) : t)
      | target == h = Just pw
      | otherwise = findHash target t

lookupInit :: RainbowTable -> B.ByteString -> Maybe [String]
lookupInit rt hashed =
  let mFound = HashMap.lookup hashed rt.table
      l = [tryFindInit rt hashed i rt.chainLength | i <- [rt.chainLength, rt.chainLength - 1 .. 1]]
   in case mFound of
        Nothing -> sequence $ filter isJust l
        Just init -> Just [init]

tryFindInit :: RainbowTable -> B.ByteString -> Int -> Int -> Maybe String
tryFindInit rt hashed i max =
  let i1 = fromIntegral i :: Integer
      max1 = fromIntegral max :: Integer
      steps = map rt.reduce [i1 .. max1 - 1]
      lastHash = foldl (\bs r -> rt.hash $ r bs) hashed steps
      mFound = HashMap.lookup lastHash rt.table
   in mFound