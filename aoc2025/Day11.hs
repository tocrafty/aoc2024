module Main where

import Parser
import Control.Applicative
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST
import Control.Monad.State
import Data.Maybe
import Debug.Trace

trace' = const id

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let kvs = fromJust $ evalStateT (runParser parseInput) contents
  print $ runST $ do
    tree <- HT.new
    sequence_ $ uncurry (HT.insert tree) <$> kvs
    part1 <- pathes tree "you"
    svr2fft <- targetBypass tree [] "fft" "dac" "svr"
    svr2dac <- targetBypass tree [] "dac" "fft" "svr"
    fft2dac <- targetBypass tree [] "dac" "svr" "fft"
    dac2fft <- targetBypass tree [] "fft" "svr" "dac"
    fft2out <- targetBypass tree [] "out" "dac" "fft"
    dac2out <- targetBypass tree [] "out" "fft" "dac"
    let part2 = svr2fft * fft2dac * dac2out + svr2dac * dac2fft * fft2out
    return $ (part1, part2)

pathes :: HT.HashTable s [Char] [[Char]] -> [Char] -> ST s Int
pathes tree val
  | val == "out" = return 1
  | otherwise = do
    mvals <- HT.lookup tree val
    maybe (return 0) (fmap sum . traverse (pathes tree)) mvals

targetBypass tree visited target bypass val = do
  caches <- HT.new
  targetBypass' tree caches visited target bypass val

targetBypass' :: HT.HashTable s [Char] [[Char]] -> HT.HashTable s [Char] Int -> [[Char]] -> [Char] -> [Char] -> [Char] -> ST s Int
targetBypass' tree caches visited target bypass val
  | val == target = return 1
  | val == bypass = return 0
  | val `elem` visited = return 0
  | otherwise = trace' ("target: " ++ target ++ " bypass: " ++ bypass ++ " curr: " ++ val) $ do
    cache <- HT.lookup caches val
    maybe (do
      mvals <- HT.lookup tree val
      n <- maybe (return 0) (fmap sum . traverse (targetBypass' tree caches (val:visited) target bypass)) mvals
      HT.insert caches val n
      return n
      ) return cache

parseInput :: Parser [([Char], [[Char]])]
parseInput = do
  firstLine <- parseLine
  lines <- many $ (char '\n' *> parseLine)
  return $ firstLine : lines
 where
  parseDevice = sequence $ [alpha, alpha, alpha]
  parseLine = do
    key <- parseDevice
    str ": "
    val <- parseDevice
    vals <- many $ (char ' ' *> parseDevice)
    return $ (key, val : vals)
