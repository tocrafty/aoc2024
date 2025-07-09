module Main where

import Data.Char (digitToInt, intToDigit)
import qualified Data.HashMap.Strict as M
import Control.Monad.State

main :: IO ()
main = do
  ns :: [Int] <- (fmap read . words) <$> readFile "./exe/Day11/input"
  let total n = evalState (memBlinks n ns) (M.fromList $ zip [0..n] $ repeat M.empty)
  print $ total 25
  print $ total 75

memBlink :: Int -> Int -> State (M.HashMap Int (M.HashMap Int Int)) Int
memBlink 0 _ = return 1
memBlink n x = do
  cache <- get
  case M.lookup n cache >>= M.lookup x of
    Just xs -> return xs
    Nothing -> if x == 0 then memBlink (n-1) 1
               else let
                 xs = digitToInt <$> show x
                 half = length xs `div` 2
               in if length xs `mod` 2 == 0 then do
                 res <- memBlinks (n-1) $ (read . fmap intToDigit) <$> [take half xs, drop half xs]
                 modify (M.alter (M.insert x res <$>) n)
                 return res
               else do
                 res <- memBlink (n-1) (x*2024)
                 modify (M.alter (M.insert x res <$>) n)
                 return res

memBlinks :: Int -> [Int] -> State (M.HashMap Int (M.HashMap Int Int)) Int
memBlinks n xs = sum <$> mapM (memBlink n) xs
