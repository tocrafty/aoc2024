module Main where

import qualified Data.Array as A
import qualified Control.Monad.Reader as R
import Data.Char (isDigit, digitToInt)
import qualified Data.HashSet as HS
import qualified Data.HashSet as HS

main :: IO ()
main = do
  contents <- (fmap . fmap) char2Tile <$> lines <$> readFile "./exe/Day10/input"
  let mp = A.listArray ((1,1), (length contents, length $ head contents)) (concat contents)
  print $ sum $ fmap length $ flip R.runReader mp $ sequence ((\(pos, _) -> climb9 pos 0) <$> A.assocs mp)

type Map = A.Array Pos Tile

type Pos = (Int, Int)

data Tile = Na | Height Int deriving Eq

char2Tile :: Char -> Tile
char2Tile c
  | isDigit c = Height $ digitToInt c
  | otherwise = Na

climb9 :: Pos -> Int -> R.Reader Map (HS.HashSet Pos)
climb9 p@(x,y) h = do
  mp <- R.ask
  if not $ A.inRange (A.bounds mp) p then return HS.empty
  else if mp A.! p /= Height h then return HS.empty
  else if mp A.! p == Height 9 then return $ HS.singleton p
  else foldl' HS.union HS.empty <$> traverse (\p -> climb9 p (h+1)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]


