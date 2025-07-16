module RainbowTable (genChain, genChains, genTable) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.ByteString qualified as B
import Data.HashMap.Strict
import HashReduce (hash, reduce)
import System.Random
import Data.HashMap.Strict qualified as HashMap

-- Gera uma string aleatória de `charCount` caracteres da lista `charset`.
genRandStr :: [Char] -> StdGen -> Int -> (String, StdGen)
genRandStr charset randSeed charCount =
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

-- Gera uma cadeia de aplicações de HashReduce. Retorna o último hash e a primeira senha
genChain :: [Char] -> StdGen -> Int -> Int -> ((B.ByteString, String), StdGen)
genChain charset randSeed pwSize chainSize =
  let (initalPw, nextGen) = genRandStr charset randSeed pwSize

      hashReduce i = do
        currPw <- get
        let hashed = hash currPw
            reduced = reduce' i hashed
        put reduced

      lastPw = flip execState initalPw $ sequence [hashReduce $ fromIntegral i | i <- [0 .. chainSize - 1]]
      lastHash = hash lastPw
   in ((lastHash, initalPw), nextGen)
  where
    reduce' = reduce charset pwSize

-- Gera uma lista de `chainCount` cadeias de aplicações de HashReduce.
genChains :: (StdGen -> ((B.ByteString, String), StdGen)) -> StdGen -> Int -> ([(B.ByteString, String)], StdGen)
genChains genChain randSeed chainCount =
  let a = do
        seed <- get
        let (row, nextSeed) = genChain seed
        put nextSeed
        return row
   in flip runState randSeed $ replicateM chainCount a

genTable :: (StdGen -> ([(B.ByteString, String)], StdGen)) -> StdGen -> (HashMap B.ByteString String, StdGen)
genTable genChains randSeed =
  let (seeds, nextSeed) = genChains randSeed
   in (HashMap.fromList seeds, nextSeed)