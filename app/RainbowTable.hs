{-# LANGUAGE OverloadedRecordDot #-}

module RainbowTable (genTable, lookup, genRandStrImpl, RainbowTable (..)) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.ByteString qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import System.Random
import Prelude hiding (lookup)
import Data.Maybe
import Debug.Trace (trace)
import Data.List (sort, group)

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
      genChains = map (\s -> s <$ last (genChain s))
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
lookup rt hashed = case HashMap.lookup hashed rt.table of
  Just initialPw -> Just $ snd . last $ rt.genChain initialPw
  Nothing ->
    let len = fromIntegral rt.chainLength :: Integer
        reduceHash i h = rt.hash $ rt.reduce i h
        reducedHashed i = composeNtimes reduceHash len i
        lastHashes = map (`reducedHashed` hashed) [len,len-1..1]
        initialPws = mapMaybe (\h -> HashMap.lookup h rt.table) lastHashes
     in trace (show initialPws) $ msum $ map (go hashed . rt.genChain) initialPws
    where
      go _ [] = Nothing
      go target ((hashed, pw) : t) =
        if hashed == target
          then Just pw
          else go target t
        
      composeNtimes :: (Integer -> B.ByteString -> B.ByteString) -> Integer -> Integer -> B.ByteString -> B.ByteString
      composeNtimes f len i 
        | len > i = composeNtimes f (len-1) i . f len
        | len == i = f len
        | otherwise = error "len < i"