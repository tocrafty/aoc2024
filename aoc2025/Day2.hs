module Main where
import Parser
import Control.Applicative
import Control.Monad.State 
import Data.Maybe
import Data.Array

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let ranges = fromJust $ evalStateT (runParser parseInput) contents
  print $ sum $ filter isInvalid $ concat $ range <$> ranges
  print $ sum $ filter (isRepeated . show) $ concat $ range <$> ranges

isInvalid :: Int -> Bool
isInvalid n = let
  ns = show n
  len = length ns
 in
  len `mod` 2 == 0 && take (div len 2) ns == drop (div len 2) ns

isRepeated :: Eq a => [a] -> Bool
isRepeated as = any id $ (\r -> maybe False (>=2) (repeatTimes r as)) <$> (flip take as <$> [1..length as])
 where
  repeatTimes r [] = Just 0
  repeatTimes r as = let lr = length r in
    if take lr as == r
    then (+1) <$> repeatTimes r (drop lr as)
    else Nothing

parseInput :: Parser [(Int, Int)]
parseInput = liftA2 (:) parseOne (some $ char ',' *> parseOne)
 where
  parseOne = do
    lft <- num
    _ <- char '-'
    rht <- num
    return (lft, rht)
