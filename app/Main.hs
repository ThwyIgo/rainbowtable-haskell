module Main (main) where

import Control.Monad (forever)
import Data.HashMap.Strict qualified as HashMap
import HashReduce (hash, reduce, s2bs)
import RainbowTable (RainbowTable (table))
import RainbowTable qualified
import Storage (load, save)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)

pwLength :: Int
pwLength = 5

chainLength :: Int
chainLength = 1000

chainCount :: Int
chainCount = 10 ^ 5

charset :: [Char]
charset =
  let cs = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['+', '-', '/', '\\', '[', ']', '*', '-']
   in if length cs /= 70
        then error "charset precisa ter exatamente 70 elementos"
        else cs

genRandStr = RainbowTable.genRandStrImpl

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["gen", path] -> do
      putStrLn "Gerando Rainbowtable..."
      let !(rt, _) = RainbowTable.genTable genRandStr hash reduce charset pwLength chainLength chainCount (mkStdGen 0)
      putStrLn "Rainbowtable gerada!"
      putStrLn $ "chainCout: " ++ show chainCount ++ ". Entradas na rainbowtable: " ++ show (length . HashMap.toList $ RainbowTable.table rt)
      save path rt
      putStrLn $ "Rainbowtable salva em: " ++ path
    ["lookup", path] -> do
      putStrLn $ "Carregando rainbowtable de: " ++ path
      rt <- load path hash reduce charset pwLength
      putStrLn "Rainbowtable carregada!"
      forever $ do
        putStr "Digite um SHA512: "
        hFlush stdout
        hashed <- s2bs <$> getLine
        let mpw = RainbowTable.lookup rt hashed
        case mpw of
          Just pw -> do
            putStrLn $ "Senha encontrada: " ++ pw
          Nothing -> do
            putStrLn "Não encontrado"
    _ -> putStrLn "Uso: rainbowtable <gen|lookup> <caminho>"
