module Main where
import Parser
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad.State
import GHC.Data.Maybe
import Control.Monad

main :: IO ()
main = do
  contents <- readFile "aoc2015/input"
  let ops = fromJust $ evalStateT (runParser parseInput) contents
  print $ foldr ($) 0 ops
  print $ snd $ execState (runMaybeT (sequence $ fmap operate (zip [1..] ops))) (0, 0)

operate :: (Int, Int -> Int) -> MaybeT (State (Int, Int)) ()
operate (i, op) = do
  (n, _) <- get
  guard (n /= -1)
  put $ (op n, i)
  return ()

parseInput :: Parser [Int -> Int]
parseInput = some $ const (+1) <$> char '(' <|> const (subtract 1) <$> char ')'
