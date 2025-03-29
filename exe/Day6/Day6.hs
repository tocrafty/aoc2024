module Day6 where

import Data.Array.MArray
import Control.Monad.ST
import Data.Array.ST
import Control.Monad.State
import GHC.Arr (boundsSTArray, writeSTArray, readSTArray)
import Data.List

main :: IO ()
main = do
  s <- readFile "./exe/Day6/input"
  let ss = lines s
  print $ runST $ do
    arr <- newListArray ((1,1), (length ss, length $ head ss)) (concat ss)
    assocs <- getAssocs arr
    let Just (x,y) = fst <$> find ((`elem` "^><v") . snd) assocs
    c <- readSTArray arr (x,y)
    (a, _) <- execStateT run (arr, Guard (x,y) (dirFrom c))
    assocs <- getAssocs a
    return $ length $ filter ((=='X').snd) assocs 

data Guard = Guard (Int, Int) Dir

data Dir = Lft | Rht | Up | Down

dirFrom :: Char -> Dir
dirFrom 'v' = Down
dirFrom '<' = Lft
dirFrom '>' = Rht
dirFrom '^' = Up
dirFrom _ = undefined

type Map s = STArray s (Int, Int) Char

type St s a = StateT (Map s, Guard) (ST s) a

run :: St s ()
run = do
  mark
  move
  out <- end
  if out then return ()
  else onBlock >> run

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
  if block == '#' then do
    put (mp, Guard (x,y) (turnBack dir))
    move
    (mp, Guard (x,y) dir) <- get
    put (mp, Guard (x,y) (turnLft dir))
  else mark

mark :: St s ()
mark = do
  (mp, guard@(Guard (x, y) _)) <- get
  lift $ writeSTArray mp (x,y) 'X'
  put (mp, guard)

turnRht :: Dir -> Dir
turnRht Lft = Up
turnRht Up = Rht
turnRht Rht = Down
turnRht Down = Lft

turnBack :: Dir -> Dir
turnBack = turnRht . turnRht

turnLft :: Dir -> Dir
turnLft = turnRht . turnBack