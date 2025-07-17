module Main where

import Parser
import Control.Applicative
import Control.Monad.State (evalStateT)
import Data.Maybe

main :: IO ()
main = do
  contents <- readFile "./exe/Day13/input" 
  let Just claws = evalStateT (runParser parseInput) contents
  print $ sum $ fromMaybe 0 . tokens <$> claws
  print $ sum $ fromMaybe 0 . tokens . (\(b1, b2, x, y) -> (b1, b2, 10000000000000+x, 10000000000000+y)) <$> claws

tokens :: (Button, Button, Int, Int) -> Maybe Int
tokens ((ax, ay), (bx, by), x, y) = let
  an = (x*by-y*bx)
  ad = (ax*by-ay*bx)
  n = an `div` ad
  m = (x-ax*n) `div` bx
  in if an `mod` ad == 0 then Just $ 3 * n + m
  else Nothing

type Button = (Int, Int)

parseInput :: Parser [(Button, Button, Int, Int)]
parseInput = many $ parseClaw <* many (char '\n')

parseClaw :: Parser (Button, Button, Int, Int)
parseClaw = (,,,)
  <$> ((,) <$> (str "Button A: X+" *> num <* str ", Y+") <*> (num <* char '\n'))
  <*> ((,) <$> (str "Button B: X+" *> num <* str ", Y+") <*> (num <* char '\n'))
  <*> (str "Prize: X=" *> num <* str ", Y=")
  <*> (num)
