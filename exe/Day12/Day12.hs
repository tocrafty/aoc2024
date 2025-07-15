{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Internal.Strict as HM
import Control.Monad.State (State, get, modify, execState)
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Generics (Generic)
import Data.List (partition, sort)
import qualified Data.Array as A

main :: IO ()
main = do
  contents <- lines <$> readFile "./exe/Day12/input"
  let arr = A.listArray ((1,1), (length contents, length $ head contents)) $ concat contents
  print $ sum $ fmap price $ foldr (<>) [] $ execState (mapM addPlot $ A.assocs arr) HM.empty
  print $ sum $ fmap price' $ foldr (<>) [] $ execState (mapM addPlot $ A.assocs arr) HM.empty

type Garden = HM.HashMap Char [Region]

type Region = (Boundaries, Int)

type Boundaries = HS.HashSet Border

data Border' = Hor (Int, Int) | Ver (Int, Int) deriving (Eq, Show, Generic)

instance Hashable Border'

data Border = B Bool Border' deriving (Show, Generic)

instance Eq Border where
  (B _ b1) == (B _ b2) = b1 == b2

instance Hashable Border where
  hashWithSalt salt (B _ b) = hashWithSalt salt b

data OrientedBorder = OB Bool Border' deriving (Eq, Show, Generic)

instance Hashable OrientedBorder

price :: Region -> Int
price (bs, n) = n * HS.size bs

price' :: Region -> Int
price' (bs, n) = n * countSides (HS.toList bs)

near :: (Int, Int) -> Boundaries -> [Border]
near pos bs = filter (flip HS.member bs) (singleBoundaries pos)

merge :: (Int, Int) -> [Region] -> [Region]
merge pos rs = let
  (neighbors, strangers) = partition ((/=0) . length . snd) $ zip rs $ (near pos . fst) <$> rs
  fuse hs ((bs, _), dels) = (foldr HS.delete (HS.union hs bs) dels)
  group = (foldl fuse (HS.fromList $ singleBoundaries pos) neighbors, 1 + sum (snd . fst <$> neighbors))
  in group : (fst <$> strangers)

addPlot :: ((Int, Int), Char) -> State Garden ()
addPlot (pos, plant) = do
  g <- get
  case HM.lookup plant g of
    Nothing -> modify (HM.insert plant [(HS.fromList $ singleBoundaries pos, 1)])
    Just _ -> modify (HM.adjust (merge pos) plant)

singleBoundaries :: (Int, Int) -> [Border]
singleBoundaries (x,y) = [B False $ Hor (x,y),
                          B True $ Hor (x+1,y),
                          B False $ Ver (x,y),
                          B True $ Ver (x,y+1)]

countSides :: [Border] -> Int
countSides bs = sum $ (countContinuations . sort) <$> groupBy bs borderKV where
  groupBy :: Hashable k => [a] -> (a -> (k, v)) -> HM.HashMap k [v]
  groupBy as f = foldr (\a -> let (k, v) = f a in HM.insertWith (<>) k [v]) HM.empty as
  countContinuations :: [Int] -> Int
  countContinuations [] = 0
  countContinuations [_] = 1
  countContinuations (x:y:zs) | x+1 == y = countContinuations (y:zs)
                              | otherwise = 1 + countContinuations (y:zs)
  borderKV :: Border -> (OrientedBorder, Int)
  borderKV (B b (Hor (x,y))) = (OB b $ Hor (x,0), y)
  borderKV (B b (Ver (x,y))) = (OB b $ Ver (0,y), x)
  