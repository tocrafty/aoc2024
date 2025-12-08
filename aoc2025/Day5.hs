module Main where
import Data.Array
import Parser
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad.State
import Data.List

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let (ranges, ids) = fromJust $ evalStateT (runParser parseInput) contents
  print $ length $ filter (inRanges ranges) ids
  print $ sum $ (\s -> rht s - lft s + 1) <$> execState (groupSegments $ sort $ uncurry Segment <$> ranges) []

inRanges :: [(Int, Int)] -> Int -> Bool
inRanges ranges n = any id $ fmap (flip inRange n) ranges

groupSegments :: [Segment] -> State [Segment] ()
groupSegments [] = return ()
groupSegments [s] = modify (s:)
groupSegments (s0:s1:ss) = do
  if lft s1 > rht s0 then modify (s0:) >> groupSegments (s1:ss)
  else groupSegments (s0{ rht = max (rht s0) (rht s1) } : ss)

data Segment = Segment
  { lft :: Int
  , rht :: Int
  } deriving Show

instance Eq Segment where
  s1 == s2 = lft s1 == lft s2

instance Ord Segment where
  s1 <= s2 = lft s1 <= lft s2

parseInput :: Parser ([(Int, Int)], [Int])
parseInput = do
  firstRange <- parseRange
  ranges <- some $ char '\n' *> parseRange
  char '\n'
  char '\n'
  firstID <- num
  ids <- some $ char '\n' *> num
  return (firstRange: ranges, firstID: ids)
 where
  parseRange = (,) <$> (num <* char '-') <*> num
