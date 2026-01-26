module Main where

import Parser
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe 
import Control.Monad.State
import Data.Char
import Debug.Trace

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let (nums, ops) = fromJust $ evalStateT (runParser parseInput) contents
  print $ sum $ (uncurry foldl1) <$> zip ops nums
  let (nums', ops') = fromJust $ evalStateT (runParser parseInput') contents
  print $ sum $ (uncurry foldl1) <$> zip ops' nums'

parseInput :: Parser ([[Int]], [Int -> Int -> Int])
parseInput = do
  nums <- some $ some (many (char ' ') *> num) <* many (char ' ') <* char '\n'
  ops <- some (many (char ' ') *> (char '*' <|> char '+')) <* many (char ' ') <* eof
  return (transpose nums, parseOp <$> ops)

parseInput' :: Parser ([[Int]], [Int -> Int -> Int])
parseInput' = do
  lines <- some $ some (expectChar (\x -> isDigit x || x == ' ')) <* char '\n'
  ops <- some (many (char ' ') *> (char '*' <|> char '+')) <* many (char ' ') <* eof
  return (fmap (fmap toNum) $ splitBySpaceBars lines, parseOp <$> ops)
 where
  splitBySpaceBars :: [[Char]] -> [[[Char]]]
  splitBySpaceBars = splitWhen (all (== ' ')) . transpose
  toNum :: [Char] -> Int
  toNum = read . dropWhile isSpace

parseOp :: Char -> (Int -> Int -> Int)
parseOp '*' = (*)
parseOp '+' = (+)
parseOp a = error $ "unexpect op" ++ [a]
