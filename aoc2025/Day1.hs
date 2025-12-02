module Main where
import Parser (Parser (runParser), char, num)
import Control.Applicative
import Control.Monad.State (State, MonadState (get, put), StateT (runStateT), runState, evalState, evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let dials = fromJust $ evalStateT (runParser parseDials) contents
  print $ length $ filter (== 0) $ evalState (processes dials) 50

data Dial = Lft Int | Rht Int deriving Show

processes :: [Dial] -> State Int [Int]
processes = traverse process
 where
  process :: Dial -> State Int Int
  process d = do
    n <- get
    case d of
      (Lft d) -> let n' = (n-d) `mod` 100 in put n' >> return n'
      (Rht d) -> let n' = (n+d) `mod` 100 in put n' >> return n'

parseDials :: Parser [Dial]
parseDials = do
  dials <- many (parseDial <* char '\n')
  dial <- parseDial
  return $ dials ++ [dial]
 where
  parseDial = ((const Lft <$> char 'L') <|> (const Rht <$> char 'R')) <*> num