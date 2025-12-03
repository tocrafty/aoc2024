module Main where
import Parser (Parser (runParser), char, num)
import Control.Applicative
import Control.Monad.State (State, MonadState (get, put), StateT (runStateT), runState, evalState, evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let dials = fromJust $ evalStateT (runParser parseDials) contents
  print $ length $ filter id $ evalState (processes dialEndAt0 dials) 50
  print $ sum $ evalState (processes dialThrough0 dials) 50

data Dial = Lft Int | Rht Int deriving Show

processes :: (Int -> Dial -> (Int, a)) -> [Dial] -> State Int [a]
processes dialOnce = traverse process
 where
  process d = do
    n <- get
    let (n', a) = dialOnce n d
    put n'
    return a

dialEndAt0 :: Int -> Dial -> (Int, Bool)
dialEndAt0 n (Lft d) = let n' = (n-d) `mod` 100 in (n', n' == 0)
dialEndAt0 n (Rht d) = let n' = (n+d) `mod` 100 in (n', n' == 0)

dialThrough0 :: Int -> Dial -> (Int, Int)
dialThrough0 n (Lft d) = let (_, div') = dialThrough0 ((100-n) `mod` 100) (Rht d) in ((n-d) `mod` 100, div')
dialThrough0 n (Rht d) = ((n+d) `mod` 100, (n+d) `div` 100)

parseDials :: Parser [Dial]
parseDials = do
  dials <- many (parseDial <* char '\n')
  dial <- parseDial
  return $ dials ++ [dial]
 where
  parseDial = ((const Lft <$> char 'L') <|> (const Rht <$> char 'R')) <*> num