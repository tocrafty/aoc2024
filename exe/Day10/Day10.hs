{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Array as A
import qualified Control.Monad.Reader as R
import Data.Char (isDigit, digitToInt)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Monoid (Sum(..))

main :: IO ()
main = do
  contents <- (fmap . fmap) char2Tile <$> lines <$> readFile "./exe/Day10/input"
  let mp = A.listArray ((1,1), (length contents, length $ head contents)) (concat contents)
  print $ sum $ fmap length $ flip R.runReader mp $ sequence ((\(pos, _) -> (climb9 pos 0)::R.Reader Map (HS.HashSet Pos)) <$> A.assocs mp)
  print $ sum $ flip R.runReader mp $ sequence ((\(pos, _) -> (climb9 pos 0):: R.Reader Map (Sum Int)) <$> A.assocs mp)

type Map = A.Array Pos Tile

type Pos = (Int, Int)

data Tile = Na | Height Int deriving Eq

char2Tile :: Char -> Tile
char2Tile c
  | isDigit c = Height $ digitToInt c
  | otherwise = Na

climb9 :: Monoid' m a => Pos -> Int -> R.Reader Map (m a)
climb9 p@(x,y) h = do
  mp <- R.ask
  if not $ A.inRange (A.bounds mp) p then return mempty
  else if mp A.! p /= Height h then return mempty
  else if mp A.! p == Height 9 then return $ singleton p
  else foldl' (<>) mempty <$> traverse (\p -> climb9 p (h+1)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

class Monoid (m a) => Monoid' m a where
  singleton :: Pos -> m a

instance Hashable Pos => Monoid' HS.HashSet Pos where
  singleton = HS.singleton

instance Monoid' Sum Int where
  singleton = const $ Sum 1


rate9 :: Pos -> Int -> R.Reader Map Int
rate9 p@(x,y) h = do
  mp <- R.ask
  if not $ A.inRange (A.bounds mp) p then return 0
  else if mp A.! p /= Height h then return 0
  else if mp A.! p == Height 9 then return 1
  else sum <$> traverse (\p -> rate9 p (h+1)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]


