module Main (main) where

import Control.Monad (forever)
import Data.HashMap.Strict qualified as HashMap
import HashReduce (bs2s, hash, reduce, s2bs)
import RainbowTable (RainbowTable (table))
import RainbowTable qualified
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)

pwLength :: Int
pwLength = 3

chainLength :: Int
chainLength = 100

chainCount :: Int
chainCount = pwLength ^ length charset `div` chainLength ^ 7

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
  let !(rt, _) = RainbowTable.genTable genRandStr hash reduce charset pwLength chainLength chainCount (mkStdGen 0)

  putStrLn "Rainbowtable gerada!"
  putStrLn $ "chainCout: " ++ show chainCount ++ ". Entradas na rainbowtable: " ++ show (length . HashMap.toList $ RainbowTable.table rt)

  let test = fst $ head $ HashMap.toList (table rt)
  putStrLn $ "Hash a ser buscado: " ++ bs2s test
  let Just senha = RainbowTable.lookup rt test
  putStrLn $ "Senha encontrada: " ++ senha
  putStrLn $ "Hash da senha: " ++ bs2s (hash senha)

  forever $ do
    putStr "Digite um SHA512: "
    hFlush stdout
    hashed <- s2bs <$> getLine
    let mpw = RainbowTable.lookup rt hashed
    case mpw of
      Just pw -> do
        putStrLn $ "Senha encontrada: " ++ pw
        putStrLn $ "Hash da senha: " ++ bs2s (hash pw)
      Nothing -> do
        putStrLn "Não encontrado"
        a <- getLine
        print $ map snd $ RainbowTable.genChain rt a
