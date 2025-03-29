module Main where

import Parser
import Control.Applicative
import Control.Monad.State (evalStateT)
import Control.Monad.ST
import qualified Data.Array.ST as A
import Control.Monad

main :: IO ()
main = do
  contents <- readFile "./exe/Day14/input"
  let Just robots = evalStateT (runParser parseRobots) contents
  print $ product4 $ foldl' sum4 (0, 0, 0, 0) $ classify . flip simulate 100 <$> robots
  dancing 0 robots

dancing :: Int -> [Robot] -> IO ()
dancing i robots = do
  if length (filter inMiddle robots) < 400 then dancing (i+1) $ flip simulate 1 <$> robots
  else do
    putStrLn $ show i
    print $ Dance robots
    c <- getChar
    unless (c /= '\n') $ dancing (i+1) $ flip simulate 1 <$> robots
 where
  inMiddle (Robot (x,_) _) = (x < 75) && (x > 25)

wide :: Int
wide = 101

tall :: Int
tall = 103

mw :: Int
mw = wide `div` 2
mt :: Int
mt = tall `div` 2

data Robot = Robot (Int, Int) (Int, Int) deriving Show

simulate :: Robot -> Int -> Robot
simulate (Robot (x,y) (vx, vy)) t = Robot ((x+vx*t) `mod` wide, (y+vy*t) `mod` tall) (vx, vy)

classify :: Robot -> (Int, Int, Int, Int)
classify (Robot (x,y) _)
  | x < mw && y < mt = (1, 0, 0, 0)
  | x > mw && y < mt = (0, 1, 0, 0)
  | x < mw && y > mt = (0, 0, 1, 0)
  | x > mw && y > mt = (0, 0, 0, 1)
  | otherwise = (0, 0, 0, 0)

sum4 :: Num a => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
sum4 (a1, a2, a3, a4) (b1, b2, b3, b4) = (a1+b1, a2+b2, a3+b3, a4+b4)

product4 :: Num a => (a,a,a,a) -> a
product4 (a,b,c,d) = a*b*c*d

parseRobot :: Parser Robot
parseRobot = Robot <$> ((,) <$> (str "p=" *> num <* char ',') <*> (num <* str " v="))
                   <*> ((,) <$> (num <* char ',') <*> (num))

parseRobots :: Parser [Robot]
parseRobots = (<>) <$> some (parseRobot <* char '\n') <*> ((:[]) <$> parseRobot <* eof)

newtype Dance = Dance [Robot]

instance Show Dance where
  show (Dance robots)= runST $ do
    arr <- A.newListArray ((0,0), (tall-1,wide)) $ repeat ' ' :: ST s (A.STArray s (Int, Int) Char)
    mapM_ (flip (A.writeArray arr) '\n') $ fmap (,wide) [0..tall-1]
    mapM_ (flip (A.writeArray arr) '*') $ pos <$> robots
    A.getElems arr
   where
    pos (Robot (x,y) _) = (y,x)
    
