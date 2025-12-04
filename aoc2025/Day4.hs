module Main where
import Parser
import Control.Applicative
import Data.Array as A
import GHC.Iface.Ext.Fields (readField)
import Control.Monad.State
import Data.Maybe
import Data.Array.ST as STA
import Control.Monad.ST
import GHC.Arr
import qualified Data.Array as STA
import qualified GHC.Arr as STA
import GHC.Data.StringBuffer (StringBuffer(len))

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let
    mp' = fromJust $ evalStateT (runParser parseInput) contents
    mp = A.listArray ((0, 0), (length mp' - 1, length (head mp') -1)) $ concat mp'
  print $ length $ filter (\(pos, a) -> a == '@' && surroundings mp pos < 4) $ A.assocs mp
  print $ runST $ do
    mp <- newListArray ((0,0), (length mp' - 1, length (head mp') - 1)) $ concat mp'
    mp <- execStateT sweeps mp
    cs <- getElems mp
    return $ count contents - count cs
   where
    count = length . filter (== '@')

surroundings :: A.Array (Int, Int) Char -> (Int, Int) -> Int
surroundings mp (x,y) = length $ filter id $ isExpect <$> [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
 where
  isExpect pos =
    if A.inRange (A.bounds mp) pos
    then mp ! pos == '@'
    else False

sweeps :: StateT (STA.STArray s (Int, Int) Char) (ST s) ()
sweeps = do
  n <- sweep
  if n == 0 then return ()
  else sweeps

sweep :: StateT (STA.STArray s (Int, Int) Char) (ST s) Int
sweep = do
  mp <- get
  posChars <- lift $ getAssocs mp
  posN <- lift $ sequence $ ((\p -> (p, ) <$> surroundings mp p) . fst) <$> filter ((== '@') . snd) posChars
  let posN' = fst <$> filter ((<4) . snd) posN
  _ <- lift $ sequence $ fmap (flip (writeSTArray mp) '.') posN'
  return $ length posN'
 where
  surroundings :: STA.STArray s (Int, Int) Char -> (Int, Int) -> ST s Int
  surroundings mp (x,y) = do
    expects <- sequence $ isExpect <$> [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
    return $ length $ filter id expects
   where
    isExpect pos = do
      if inRange (STA.boundsSTArray mp) pos
      then fmap (== '@') $ readSTArray mp pos
      else return False

parseInput :: Parser [[Char]]
parseInput = do
  firstLine <- parseLine
  otherLines <- many $ char '\n' *> parseLine
  return $ firstLine : otherLines
 where
  parseLine = some $ char '.' <|> char '@'
