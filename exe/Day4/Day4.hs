module Day4 where

import Control.Monad.Reader
import Data.Array
import Control.Arrow

main :: IO ()
main = do
    s <- readFile "./exe/Day4/input"
    let rows = lines s
    let end = (length rows, length $ head rows)
    let countAll = (length . filter id) <$> sequence (fmap xmas $ range ((1,1), end))
    putStrLn $ show $ runReader countAll $ listArray ((1,1), end) (concat rows)

main' :: IO ()
main' = do
    s <- readFile "./exe/Day4/input"
    let rows = lines s
    let end = (length rows, length $ head rows)
    let countAll = sum
               <$> (sequence . fmap (uncurry $ count "XMAS")) (
                    (,) <$> (range ((1,1),end))
                        <*> ((.) <$> fmap first [(+1),(-1+),id]
                                 <*> fmap second [(+1),(-1+),id]))
    putStrLn $ show $ runReader countAll $ listArray ((1,1), end) (concat rows)

count :: String -> Pos -> (Pos -> Pos) -> Reader (Array (Int, Int) Char)  Int
count [] _ _ = return 0
count [x] p _ = do
    ss <- ask
    if (inRange (bounds ss) p) && ss ! p == x
    then return 1
    else return 0
count (c:cs) p next = do
    ss <- ask
    if (inRange (bounds ss) p) && ss ! p == c
    then count cs (next p) next
    else return 0

xmas :: Pos -> Reader (Array (Int, Int) Char) Bool
xmas (x,y) = do
    ss <- ask
    return $ posIs ss (x,y) 'A'
          && (posIs ss (x-1,y-1) 'M' && posIs ss (x+1,y+1) 'S'
           || posIs ss (x-1,y-1) 'S' && posIs ss (x+1,y+1) 'M')
          && (posIs ss (x-1,y+1) 'M' && posIs ss (x+1,y-1) 'S'
           || posIs ss (x-1,y+1) 'S' && posIs ss (x+1,y-1) 'M')
  where
    posIs ss p c =
        if (inRange (bounds ss) p)
        then ss ! p == c
        else False

type Pos = (Int, Int)
