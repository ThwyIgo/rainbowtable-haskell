module Main (main) where

import Control.Monad (forM, forever, replicateM)
import Control.Monad.State ( runState, MonadState(put, get) )
import Data.HashMap.Strict qualified as HashMap
import HashReduce (bs2s, hash, reduce, s2bs)
import RainbowTable (RainbowTable (table))
import RainbowTable qualified
import Storage (load, save)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)

pwLength :: Int
pwLength = 5

chainLength :: Int
chainLength = 200

chainCount :: Int
chainCount = 10 ^ 6

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
      putStrLn $ "chainCout: " ++ show chainCount ++ ". Entradas na rainbowtable: " ++ show (HashMap.size $ RainbowTable.table rt)
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
    ["test", path] -> do
      putStrLn $ "Carregando rainbowtable de: " ++ path
      rt <- load path hash reduce charset pwLength
      putStrLn "Rainbowtable carregada!"
      let (randPws, _) = flip runState (mkStdGen 1) $ replicateM 1000 $ do
            seed <- get
            let (str, nextSeed) = genRandStr charset seed pwLength
            put nextSeed
            return str
      results <- forM randPws $ \pw -> do
        putStrLn $ "Procurando senha: " ++ pw
        let hashed = hash pw
            mFound = RainbowTable.lookup rt hashed
        putStrLn $ "Hash a ser buscado: " ++ bs2s hashed
        case mFound of
          Just found -> do
            putStrLn $ "Sucesso! Senha encontrada: " ++ found
            return True
          Nothing -> do
            putStrLn "Falhou"
            return False
      let rate :: Float = fromIntegral (length (filter id results)) / fromIntegral (length results)
      putStrLn $ "Senhas encontradas: " ++ show (rate * 100) ++ "%"
    _ -> putStrLn "Uso: rainbowtable <gen|lookup> <caminho>"
