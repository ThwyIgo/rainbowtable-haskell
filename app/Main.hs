module Main (main) where

import qualified RainbowTable
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)
import HashReduce (hash, reduce, bs2s)
import Data.HashMap.Strict (toList)
import RainbowTable (RainbowTable(table))

pwSize :: (Integral a) => a
pwSize = 5

chainSize :: (Integral a) => a
chainSize = 100

chainCount :: (Integral a) => a
chainCount = 2000

charset :: [Char]
charset =
  let cs = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['+', '-', '/', '\\', '[', ']', '*', '-']
   in if length cs /= 70
        then error "charset precisa ter exatamente 70 elementos"
        else cs

genRandStr = RainbowTable.genRandStrImpl

main :: IO ()
main = do
  putStrLn "Gerando Rainbowtable..."
  let !(rt, _) = RainbowTable.genTable genRandStr hash reduce charset pwSize chainSize chainCount (mkStdGen 0)

  putStrLn "Rainbowtable gerada!"

  let test = fst $ head $ toList (table rt)
  putStrLn $ "Hash a ser buscado: " ++ bs2s test
  let Just senha = RainbowTable.lookup rt test
  putStrLn $ "Senha encontrada: " ++ senha
  putStrLn $ "Hash da senha: " ++ bs2s (hash senha)