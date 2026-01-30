module Main where

import Control.Monad.State
import Parser
import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let grid = fromJust $ evalStateT (runParser paserInput) contents
  print $ evalState (scatters $ drop 1 grid) (head grid)

data Cell = Beam | Splitter | Empty deriving (Eq, Show)

type Grid = [[Cell]]

paserInput :: Parser Grid
paserInput = do
  rows <- some $ some (char '|' <|> char '^' <|> char '.' <|> char 'S') <* char '\n'
  eof
  return $ fmap (fmap parseCell) rows
 where
  parseCell :: Char -> Cell
  parseCell '|' = Beam
  parseCell '^' = Splitter
  parseCell '.' = Empty
  parseCell 'S' = Beam
  parseCell c = error $ "unexpected cell: " ++ [c]

scatters :: [[Cell]] -> State [Cell] Int
scatters [] = return 0
scatters (cs:css) = do
  beams <- get
  let (n, beams') = scatter beams cs
  put beams'
  n' <- scatters css
  return (n + n')

scatter :: [Cell] -> [Cell] -> (Int, [Cell])
scatter beams cells = let
  c3 = uncurry scatterCell <$> zip beams cells
 in
  (length $ filter (== (Beam, Empty, Beam)) c3, 
  mergeC3 <$> zip3 (thd3 <$> (Empty, Empty, Empty) : c3) (snd3 <$> c3) (fst3 <$> drop 1 c3 ++ [(Empty, Empty, Empty)]))
 where
  mergeC3 :: (Cell, Cell, Cell) -> Cell
  mergeC3 (Beam, _, _) = Beam
  mergeC3 (_, Beam, _) = Beam
  mergeC3 (_, _, Beam) = Beam
  mergeC3 _ = Empty
  scatterCell :: Cell -> Cell -> (Cell, Cell, Cell)
  scatterCell Beam Splitter = (Beam, Empty, Beam)
  scatterCell Beam Empty = (Empty, Beam, Empty)
  scatterCell _ _ = (Empty, Empty, Empty)
  fst3 (a,_,_) = a
  snd3 (_,b,_) = b
  thd3 (_,_,c) = c
