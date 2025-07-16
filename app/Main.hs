module Main (main) where

import qualified RainbowTable
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)
import HashReduce (hash, reduce, bs2s, s2bs)
import RainbowTable (RainbowTable(table))
import Data.HashMap.Strict qualified as HashMap
import Control.Monad (forever)

pwLength :: (Integral a) => a
pwLength = 5

chainLength :: (Integral a) => a
chainLength = 2

chainCount :: (Integral a) => a
chainCount = 5000

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

  forever $ do
    putStr "Digite um SHA512: "
    hFlush stdout
    hashed <- s2bs <$> getLine
    let mpw = RainbowTable.lookup rt hashed
    case mpw of
      Just pw -> do
        putStrLn $ "Senha encontrada: " ++ pw
        putStrLn $ "Hash da senha: " ++ bs2s (hash pw)
      Nothing -> do putStrLn "Não encontrado"
                    a <- getLine
                    print $ RainbowTable.genChain rt a
