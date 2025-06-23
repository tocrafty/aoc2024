module Main where

import Parser
import Control.Applicative
import Control.Monad.State (evalStateT)
import GHC.Utils.Misc

main :: IO ()
main = readFile "./exe/Day7/input" >>= print . show
     . (fmap (sum
             . (fmap fstOf3)
             . (filter (uncurry3 isExpEqAny))
             . (fmap (third3 allCombinations)))
       . evalStateT (runParser parseInput))

isExpEqAny :: Integer -> Integer -> [[Integer -> Integer]] -> Bool
isExpEqAny target initial fss = any (isExpEq target initial) fss

isExpEq :: Integer -> Integer -> [Integer -> Integer] -> Bool
isExpEq target initial [] = initial == target
isExpEq target initial (f:fs)
  | initial > target = False
  | otherwise = isExpEq target (f initial) fs

allCombinations :: [Integer] -> [[Integer -> Integer]]
allCombinations [] = [[]]
allCombinations (i:is) = fmap ((+i):) (allCombinations is) ++ fmap ((*i):) (allCombinations is) ++ fmap ((flip calibration i):) (allCombinations is)

calibration :: Integer -> Integer -> Integer
calibration a b = a * (10 ^ digits b) + b where
  digits :: Integer -> Integer
  digits 0 = 0
  digits n = 1 + digits (n `div` 10)

parseInput :: Parser [(Integer, Integer, [Integer])]
parseInput = some ((,,) <$> num <* char ':' <* many (char ' ') <*> num <*> (some (many (char ' ') *> num)) <* char '\n') <* eof
