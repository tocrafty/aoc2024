module Main where

import qualified Parser as P
import qualified Data.Array.ST as A
import Control.Monad.ST
import Control.Monad.State
import Control.Applicative

main :: IO ()
main = do
  contents <- readFile "./exe/Day9/input"
  let Just disk = evalStateT (P.runParser parseDisk) contents
  print $ flip evalState 0 $ checksum $ runST $ do
    d <- A.newListArray (1, length disk) disk
    compactLft d (1, length disk)
  let Just disk = evalStateT (P.runParser parseFreeUsedDisk) contents
  print $ flip evalState 0 $ checksum $ (concat . fmap toBlocks) $ runST $ do
    d <- A.newListArray (1, length disk) disk
    disk <- compactMostFit (length disk) d
    A.getElems disk

data Block = File Int | Hole deriving Show
type Blocks = (Int, Block)

type Disk s = A.STArray s Int Blocks

parseDisk :: P.Parser [Blocks]
parseDisk = flip parseDisk' 0 <$> (some P.digit <* P.eof) where
  parseDisk' :: [Int] -> Int -> [Blocks]
  parseDisk' [] _ = []
  parseDisk' (x:xs) pos
    | mod pos 2 == 0 = (x, File $ div pos 2) : parseDisk' xs (pos+1)
    | otherwise = (x, Hole) : parseDisk' xs (pos+1)

checksum :: [Blocks] -> State Int Int
checksum [] = return 0
checksum ((n, b):blocks) = do
  pos <- get
  modify (+n)
  case b of
    Hole -> checksum blocks
    File f -> ((pos*2+n-1)*n*f`div`2+) <$> checksum blocks

compactLft :: Disk s -> (Int, Int) -> ST s [Blocks]
compactLft d (i,j)
  | i > j = return []
  | otherwise = do
    (ln, lb) <- A.readArray d i
    case lb of
      Hole -> compactRht d (i,j)
      _ -> ((ln, lb):) <$> compactLft d (i+1,j)

compactRht :: Disk s -> (Int, Int) -> ST s [Blocks]
compactRht d (i,j)
  | i > j = return []
  | otherwise = do
    (rn, rb) <- A.readArray d j
    case rb of
      Hole -> compactRht d (i,j-1)
      _ -> do
        (ln, _) <- A.readArray d i
        if ln == rn then ((ln,rb):) <$> compactLft d (i+1,j-1)
        else if ln < rn then do
          A.writeArray d j (rn-ln, rb)
          ((ln,rb):) <$> compactLft d (i+1,j)
        else do
          A.writeArray d i (ln-rn, Hole)
          ((rn,rb):) <$> compactLft d (i,j-1)

data FreeUsed = FreeUsed Blocks [Blocks]

type FreeUsedDisk s = A.STArray s Int FreeUsed

parseFreeUsedDisk :: P.Parser [FreeUsed]
parseFreeUsedDisk = fmap toFreeUsed <$> parseDisk

toBlocks :: FreeUsed -> [Blocks]
toBlocks (FreeUsed free used) = reverse used ++ [free]

toFreeUsed :: Blocks -> FreeUsed
toFreeUsed bs@(_, Hole) = FreeUsed bs []
toFreeUsed bs@_ = FreeUsed (0, Hole) [bs]

compactMostFit :: Int -> FreeUsedDisk s -> ST s (FreeUsedDisk s)
compactMostFit pos d = do
  bounds <- A.getBounds d
  if not $ A.inRange bounds pos then return d
  else do
    FreeUsed (free, _) uses <- A.readArray d pos
    if free > 0 then compactMostFit (pos-1) d
    else case uses of
      [use@(fn, _)] -> do
        fit 1
        compactMostFit (pos-1) d
        where
          fit i
            | i >= pos = return ()
            | otherwise = do
              FreeUsed (free, _) uses <- A.readArray d i
              if free >= fn then do
                A.writeArray d i $ FreeUsed (free-fn, Hole) (use:uses)
                A.writeArray d pos $ FreeUsed (fn, Hole) []
              else fit (i+1)
      _ -> compactMostFit (pos-1) d
 