module Main where

import Parser
import Control.Applicative
import Data.List
import Data.Maybe 
import Control.Monad.State
import Debug.Trace

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let (nums, ops) = fromJust $ evalStateT (runParser parseInput) contents
  print $ sum $ (uncurry foldl1) <$> zip ops nums

parseInput :: Parser ([[Int]], [Int -> Int -> Int])
parseInput = do
  nums <- some $ some (many (char ' ') *> num) <* many (char ' ') <* char '\n'
  ops <- some (many (char ' ') *> (char '*' <|> char '+')) <* many (char ' ') <* eof
  return (transpose nums, parseOp <$> ops)
 where
  parseOp '*' = (*)
  parseOp '+' = (+)
  parseOp a = error $ "unexpect op" ++ [a]
  
