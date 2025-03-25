{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day5 where

import Control.Applicative
import Control.Monad.State
import Parser
import Control.Arrow
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe

main :: IO ()
main =  readFile "./exe/Day5/input" >>= putStrLn . show . filterEvenSecond . evalStateT (runParser parseInput) where
    filterEvenSecond = fmap $ snd . (second $ filter (even . length))

parseInput :: Parser ([(Int,Int)], [[Int]])
parseInput = (,) <$> (some parseOrder) <* char '\n' <*> some parseUpdate <* eof

parseOrder :: Parser (Int,Int)
parseOrder = (,) <$> num <* char '|' <*> num <* char '\n'

parseUpdate :: Parser [Int]
parseUpdate = (:) <$> num <*> (many (char ',' *> num)) <* char '\n'

type HMS = HM.HashMap Int (HS.HashSet Int)

parseRules :: [(Int, Int)] -> HMS
parseRules = foldl' (\m p -> HM.insertWith HS.union (fst p) (HS.singleton $ snd p) m) (HM.empty)

validate :: [Int] -> HMS -> Bool
validate xs ms = all validate' ((,) <$> xs <*> xs) where
    validate' (a,b) = fromMaybe False $ fmap (HS.member b) (ms HM.!? a)

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

