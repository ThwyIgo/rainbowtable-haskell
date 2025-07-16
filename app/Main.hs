module Main (main) where

import qualified RainbowTable
import System.IO (hFlush, stdout)
import System.Random (mkStdGen)

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

main :: IO ()
main = do
  putStrLn "Gerando Rainbowtable..."
  let genChain randSeed = RainbowTable.genChain charset randSeed pwSize chainSize
      genChains randSeed = RainbowTable.genChains genChain randSeed chainCount
      !(!rt, _) = RainbowTable.genTable genChains (mkStdGen 0)

  putStrLn "Rainbowtable gerada!"
  putStr $ "Digite um SHA512 referente a uma senha de " ++ show pwSize ++ " caracteres: "
  hFlush stdout
  hashedPw <- getLine

  putStrLn $ "SHA512 da senha: " ++ undefined