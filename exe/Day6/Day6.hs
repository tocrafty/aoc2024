module Main where

import Data.Array.MArray
import Control.Monad.ST
import Data.Array.ST
import Control.Monad.State
import GHC.Arr (boundsSTArray, writeSTArray, readSTArray, STArray (STArray), Array (Array))
import Data.List

main :: IO ()
main = do
  contents <- readFile "./exe/Day6/input"
  print $ runST $ do
    (mp, _) <- initMap contents >>= execStateT run
    assocs <- getAssocs mp
    return $ length $ filter (hasFootprint.snd) assocs
  print $ runST $ do
    mgs <- initMap contents >>= genStones
    countTrues $ (>>= worked) <$> mgs
   where
    worked :: Maybe (Map s, Guard) -> ST s Bool
    worked mp = maybe (return False) (evalStateT loop) mp
    countTrues :: [ST s Bool] -> ST s Int
    countTrues = fmap (length . filter id) . sequence
    
data Guard = Guard (Int, Int) Dir

data Dir = Lft | Rht | Up | Down deriving Eq

data Block = Stone | D [Dir]

type Map s = STArray s (Int, Int) Block

type St s a = StateT (Map s, Guard) (ST s) a

genStones :: (Map s, Guard) -> ST s [ST s (Maybe (Map s, Guard))]
genStones (mp, guard) = do
  ((x1,y1), (x2,y2)) <- getBounds mp
  return $ uncurry addStone' <$> zip (repeat $ (,guard) <$> copyMap mp) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
 where
  addStone' :: ST s (Map s, Guard) -> (Int,Int) -> ST s (Maybe (Map s, Guard))
  addStone' = (. flip addStone) . (>>=)
  copyMap :: Map s -> ST s (Map s)
  copyMap mp = do
    frozen :: Array (Int, Int) Block <- freeze mp
    thaw frozen

addStone :: (Map s, Guard) -> (Int,Int) -> ST s (Maybe (Map s, Guard))
addStone game xy = do
  let (mp, g) = game
  block <- readSTArray mp xy
  case block of
    (D []) -> writeSTArray mp xy Stone >> (return $ Just (mp, g))
    _ -> return Nothing

initMap :: String -> ST s (Map s, Guard)
initMap contents = do
  let ss = lines contents
  arr <- newListArray ((1,1), (length ss, length $ head ss)) (toBlock <$> concat ss)
  assocs <- getAssocs arr
  let Just (x,y) = fst <$> find (hasFootprint.snd) assocs
  block <- readSTArray arr (x,y)
  case block of
    D [d] -> return (arr, Guard (x,y) d)
    _ -> error "initMap impossible"
 where
  toBlock '#' = Stone
  toBlock '^' = D [Up]
  toBlock 'v' = D [Down]
  toBlock '<' = D [Lft]
  toBlock '>' = D [Rht]
  toBlock _ = D []

hasFootprint :: Block -> Bool
hasFootprint Stone = False
hasFootprint (D []) = False
hasFootprint _ = True

run :: St s ()
run = do
  mark
  move
  out <- end
  if out then return ()
  else onBlock >> run

loop :: St s Bool
loop = do
  mark
  move
  out <- end
  if out then return False
  else do
    (mp, Guard (x,y) d) <- get
    b <- lift $ readSTArray mp (x,y)
    if isFootprint b d then return True else onBlock >> loop
 where
  isFootprint Stone _ = False
  isFootprint (D ds) d = d `elem` ds

move :: St s ()
move = do
  (mp, Guard (x, y) dir) <- get
  let
    move' Lft = put (mp, Guard (x,y-1) dir)
    move' Rht = put (mp, Guard (x,y+1) dir)
    move' Up = put (mp, Guard (x-1,y) dir)
    move' Down = put (mp, Guard (x+1,y) dir)
  move' dir

end :: St s Bool
end = do
  (mp, Guard (x, y) _) <- get
  if inRange (boundsSTArray mp) (x,y)
  then return False
  else return True

onBlock :: St s ()
onBlock = do
  (mp, Guard (x,y) dir) <- get
  block <- lift $ readSTArray mp (x,y)
  case block of
    Stone -> do
      put (mp, Guard (x,y) (turnBack dir))
      move
      (mp, Guard (x,y) dir) <- get
      put (mp, Guard (x,y) (turnLft dir))
    _ -> return ()

mark :: St s ()
mark = do
  (mp, guard@(Guard (x, y) d)) <- get
  block <- lift $ readSTArray mp (x,y)
  case block of
    D ds -> do
      lift $ writeSTArray mp (x,y) $ D (d:ds)
      put (mp, guard)
    _ -> error $ "mark impossible" ++ show (x,y)

turnRht :: Dir -> Dir
turnRht Lft = Up
turnRht Up = Rht
turnRht Rht = Down
turnRht Down = Lft

turnBack :: Dir -> Dir
turnBack = turnRht . turnRht

turnLft :: Dir -> Dir
turnLft = turnRht . turnBack