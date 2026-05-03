{-# LANGUAGE DeriveGeneric #-}

module Main where

import Parser
import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as HT
import Data.Hashable
import GHC.Generics (Generic)
import Data.Foldable
import Data.List
import Control.Monad
import Control.Monad.Extra
import Data.Ord
import Data.Function

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let ps = fromJust $ evalStateT (runParser parseInput) contents
  print $ maximum $ area <$> pair ps
  print $ maximum $ fmap area $ runST $ do
    ht <- hedges ps
    filterM (\(p1, p2) -> allM (uncurry  $ continuous ht) $ continuous2Check p1 p2 ) $ pair ps
 where
  pair [] = []
  pair (x:xs) = [(x,y) | y <- xs] ++ pair xs
  area ((x1, y1), (x2, y2)) = (abs (x1-x2) + 1) * (abs (y1-y2) + 1)
  continuous2Check (x1, y1) (x2, y2) = [ ((X, x1), (y1, y2))
                                       , ((X, x2), (y1, y2))
                                       , ((Y, y1), (x1, x2))
                                       , ((Y, y2), (x1, x2))
                                       ]

parseInput :: Parser [(Int, Int)]
parseInput = do
  p <- (,) <$> num <*> (char ',' *> num)
  ps <- some $ (,) <$> (char '\n' *> num) <*> (char ',' *> num)
  return $ p:ps

-- suppose points wraps the area clock wise.
hedges :: [(Int, Int)] -> ST s (HT.HashTable s (Axis, Int) [(Int, InnerSide)])
hedges [] = HT.new
hedges ps@(p0:ps') = do
  ht <- HT.new
  traverse_ (hedges' ht) $ zip ps (ps'++[p0])
  HT.foldM (const $ (\(k, _) -> HT.mutate ht k ((, ()) . fmap (fmap takeGroup . groupBy ((==) `on` snd) . sortBy (comparing fst))))) () ht
  return ht
 where
  hedges' ht ((x1, y1), (x2, y2))
    | x1 == x2 = traverse (flip (HT.mutate ht) (mut (x1, if y1 < y2 then Lft else Rht))) $ fmap (Y, ) [min y1 y2 .. max y1 y2]
    | y1 == y2 = traverse (flip (HT.mutate ht) (mut (y1, if x1 < x2 then Rht else Lft))) $ fmap (X, ) [min x1 x2 .. max x1 x2]
    | otherwise = undefined
   where
    mut b ma = (Just $ b : fromMaybe [] ma, ())
  takeGroup [] = undefined
  takeGroup xs@((_, Lft):_) = last xs
  takeGroup xs@((_, Rht):_) = head xs

continuous :: HT.HashTable s (Axis, Int) [(Int, InnerSide)] -> (Axis, Int) -> (Int, Int) -> ST s Bool
continuous ht axis (x0, x1) = do
  xs <- HT.lookup ht axis
  let ps = pairs $ fromMaybe [] xs
  return $ (/=0) $ length $ find (\p -> fst p <= min x0 x1 && max x0 x1 <= snd p) (ps)
 where
  pairs [] = []
  pairs [_] = undefined
  pairs ((x1, d1):(x2,d2):xs) =
    if d1 /= Rht then undefined
    else if d2 /= Lft then undefined
    else (x1,x2):pairs xs

data Axis = X | Y deriving (Show, Eq, Generic)

instance Hashable Axis

data InnerSide = Lft | Rht deriving (Show, Eq)
