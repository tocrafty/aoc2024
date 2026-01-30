module Main where

import Control.Monad.State
import Parser
import Control.Applicative
import Data.Maybe
import Data.Function

main :: IO ()
main = do
  contents <- readFile "aoc2025/input"
  let grid = fromJust $ evalStateT (runParser paserInput) contents
  print $ evalState (scatters $ drop 1 grid) (head grid)
  print $ sum $ (\case
    (Beam n) -> n
    _ -> 0
    ) <$> execState (scatters $ drop 1 grid) (head grid)

data Cell = Beam Int | Splitter | Empty deriving (Eq, Show)

isBeam :: Cell -> Bool
isBeam (Beam _) = True
isBeam _ = False

type Grid = [[Cell]]

paserInput :: Parser Grid
paserInput = do
  rows <- some $ some (char '|' <|> char '^' <|> char '.' <|> char 'S') <* char '\n'
  eof
  return $ fmap (fmap parseCell) rows
 where
  parseCell :: Char -> Cell
  parseCell '|' = Beam 1
  parseCell '^' = Splitter
  parseCell '.' = Empty
  parseCell 'S' = Beam 1
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
  (length $ filter (\case
    (Beam _, Empty, Beam _) -> True
    _ -> False
  ) c3,
  mergeC3 <$> zip3 (thd3 <$> (Empty, Empty, Empty) : c3) (snd3 <$> c3) (fst3 <$> drop 1 c3 ++ [(Empty, Empty, Empty)]))
 where
  mergeC3 :: (Cell, Cell, Cell) -> Cell
  mergeC3 (c1, c2, c3) = filter isBeam [c1, c2, c3] & \bs -> case bs of
    [] -> Empty
    _ -> Beam $ sum [n | Beam n <- bs]
  scatterCell :: Cell -> Cell -> (Cell, Cell, Cell)
  scatterCell (Beam n) Splitter = (Beam n, Empty, Beam n)
  scatterCell (Beam n) Empty = (Empty, Beam n, Empty)
  scatterCell _ _ = (Empty, Empty, Empty)
  fst3 (a,_,_) = a
  snd3 (_,b,_) = b
  thd3 (_,_,c) = c
