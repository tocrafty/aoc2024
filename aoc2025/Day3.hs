module Main where
import Parser
import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.Char

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let banks = fromJust $ evalStateT (runParser parseInput) contents
  print $ sum $ largestJoltage <$> banks
  print $ sum $ (\bank -> largestJoltages bank 0 12) <$> banks

largestJoltage :: [Int] -> Int
largestJoltage as =
  let
    maxVal = maximum (take (length as -1) as)
    (_, first:others) = span (/= maxVal) as
  in
    first * 10 + maximum others

largestJoltages :: [Int] -> Int -> Int -> Int
largestJoltages _ j 0 = j
largestJoltages xs j c = largestJoltages others (j*10 + first) (c-1)
 where
  maxVal = maximum (take (length xs - c + 1) xs)
  (_, first: others) = span (/= maxVal) xs

parseInput :: Parser [[Int]]
parseInput = do
  firstN <- num
  otherNs <- some (char '\n' *> num)
  return $ (fmap digitToInt . show) <$> firstN:otherNs
