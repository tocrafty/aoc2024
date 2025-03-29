module Day5 where

import Control.Applicative
import Control.Monad.State
import Parser
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl', tails)

main :: IO ()
main =  readFile "./exe/Day5/input" >>= (print . fmap sumValidCenter . evalStateT (runParser parseInput)) where
    sumValidCenter (orders, updates) = sum $ middle <$> filter (`validate` rules) updates where
        rules = parseRules orders

main' :: IO ()
main' =  readFile "./exe/Day5/input" >>= (print . fmap sumValidCenter . evalStateT (runParser parseInput)) where
    sumValidCenter (orders, updates) = sum $ middle . (`reorder` rules) <$> filter (not . (`validate` rules)) updates where
        rules = parseRules orders

parseInput :: Parser ([(Int,Int)], [[Int]])
parseInput = (,) <$> some parseOrder <* char '\n' <*> some parseUpdate <* eof

parseOrder :: Parser (Int,Int)
parseOrder = (,) <$> num <* char '|' <*> num <* char '\n'

parseUpdate :: Parser [Int]
parseUpdate = (:) <$> num <*> many (char ',' *> num) <* char '\n'

type Rules = HM.HashMap Int (HS.HashSet Int)

parseRules :: [(Int, Int)] -> Rules
parseRules = foldl' (\m p -> HM.insertWith HS.union (fst p) (HS.singleton $ snd p) m) HM.empty

validate :: [Int] -> Rules -> Bool
validate xs ms = all validate' [(x,y) | (x:ys) <- tails xs, y <- ys] where
    validate' (a,b) = maybe False (HS.member b) (ms HM.!? a)

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

reorder :: [Int] -> Rules -> [Int]
reorder xs r = sort xs where
    sort [] = []
    sort [x] = [x]
    sort (x:xs) = sort (filter (`validate` x) xs)
               ++ [x]
               ++ sort (filter (validate x) xs)
    validate a b = maybe False (HS.member b) (r HM.!? a)
