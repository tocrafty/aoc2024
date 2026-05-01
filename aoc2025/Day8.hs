module Main where

import Data.List
import Data.Ord
import Control.Monad.ST
import Data.Foldable.Extra
import Data.Semigroup
import qualified Heap as H
import Data.Maybe
import Control.Monad
import qualified Data.Vector.Mutable as MV
import Parser
import Control.Applicative
import Control.Monad.State (evalStateT)
import qualified Data.HashTable.ST.Basic as HM

k = 10

main :: IO ()
main = main' k

main' :: Int -> IO ()
main' k = do
  contents <- readFile "aoc2025/input"
  let ps = fromJust $ evalStateT (runParser parseInput) contents
  --print $ nearestK ps k
  --print $ reverse $ take k $ violence ps
  print $ product $ take 3 $ sortBy (flip compare) $ runST $ do
    g <- HM.new
    sequence_ $ flip (HM.insert g) [] <$> ps
    sequence_ $ (\(p1, p2, _) -> connect g p1 p2) <$> nearestK ps k
    circuit g
  print $ (\(p1, p2) -> axis X p1 * axis X p2)
        $ lastPair ps
        $ (\(p1, p2, _) -> (p1, p2)) <$> violence ps

lastPair :: [Pos] -> [(Pos, Pos)] -> (Pos, Pos)
lastPair ps ds = runST $ do
  p2c <- HM.new
  c2p <- HM.new
  sequence_ $ (\(i, p) -> HM.insert p2c p i >> HM.insert c2p i [p]) <$> zip ([1..] :: [Int]) ps
  let
    step p1 p2 = do
      c1 <- fromJust <$> HM.lookup p2c p1
      c2 <- fromJust <$> HM.lookup p2c p2
      when (c1 /= c2) $ do
        ps <- fromJust <$> HM.lookup c2p c2
        sequence_ $ flip (HM.mutate p2c) (const $ (Just c1, ())) <$> ps
        HM.mutate c2p c1 ((,()) . fmap (++ps))
        HM.delete c2p c2
      HM.size c2p
  fromJust <$> findM (\(p1, p2) -> (==1) <$> step p1 p2) ds

violence :: [Pos] -> [(Pos, Pos, Int)]
violence ps = (\(Arg _ d) -> d) <$> ds
 where
  pairs [] = []
  pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs
  disOf (p1,p2) = let d = dis p1 p2 in Arg d (p1, p2, d)
  ds = sort $ disOf <$> pairs ps

type Graph s = HM.HashTable s Pos [Pos]

connect :: Graph s -> Pos -> Pos -> ST s ()
connect g p1 p2 = do
  HM.mutate g p1 ((,()) . fmap (p2:))
  HM.mutate g p2 ((,()) . fmap (p1:))

circuit :: Graph s -> ST s [Int]
circuit g = do
  visits <- HM.new
  _ <- HM.foldM (\t (k,_) -> HM.insert t k False >> return t) visits g
  HM.foldM (\szs (p,visited) -> if visited then return szs else fmap (:szs) $ exhaust visits p) [] visits
 where
  exhaust visits p = do
    visited <- HM.lookup visits p
    if (fromMaybe True visited) then return 0
    else do
      HM.mutate visits p (const (Just True, ()))
      (1+) <$> do
        conns <- fromMaybe [] <$> HM.lookup g p
        szs <- sequence $ exhaust visits <$> conns
        return $ sum szs

type Pos = (Int, Int, Int)

dis :: Pos -> Pos -> Int
dis (x1, y1, z1) (x2, y2, z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

nearestK :: [Pos] -> Int -> [(Pos, Pos, Int)]
nearestK ps k = nearestKAxis ps X k
 where
  nearestKAxis :: [Pos] -> Axis -> Int -> [(Pos, Pos, Int)]
  nearestKAxis [] _ _ = []
  nearestKAxis (p:ps) a k = allK
   where
    (lft, rht) = partition (\p' -> axis a p' <= axis a p) ps
    lftK = nearestKAxis lft (rotate a) k
    rhtK = nearestKAxis rht (rotate a) k
    midK = if null lft && null rht then []
      else let
        radius = floor . sqrt . fromIntegral . third $ maximumBy (comparing third) (lftK <> rhtK)
        classify (ls, rs) p'
          | axis a p' < axis a p - radius  = (ls, rs)
          | axis a p' <= axis a p          = (p':ls, rs)
          | axis a p' <= axis a p + radius = (ls, p':rs)
          | otherwise                      = (ls, rs)
        (ls, rs) = if length lftK + length rhtK < k then (lft, rht)
          else foldl classify ([], []) ps
        disOf p1 p2 = let d = dis p1 p2 in Arg (Down d) (p1, p2, d)
        ds = [disOf p1 p2 | p1 <- ls, p2 <- rs] <> (disOf p <$> ls) <> (disOf p <$> rs)
        dsK = topK ds
      in argVal <$> dsK
    allK = fmap argVal . topK $ (\ds@(_, _, d) -> Arg (Down d) ds) <$> (lftK <> rhtK <> midK)
  argVal (Arg _ b) = b
  third (_,_,x) = x
  pushK :: (Ord a, MV.PrimMonad m) => H.Heap (MV.PrimState m) a -> a -> m ()
  pushK h a = do
    H.push h a
    sz <- H.length h
    when (sz == k+1) $ void $ H.pop h
  topK :: (Ord a) => [a] -> [a]
  topK ds = runST $ do
    h <- H.newHeap
    sequence_ $ pushK h <$> ds
    fmap (mapMaybe id) $ sequence $ take k $ repeat $ H.pop h

parseInput :: Parser [Pos]
parseInput = do
  p <- liftA3 (,,) (num <* char ',') (num <* char ',') num
  ps <- many $ liftA3 (,,) (char '\n' *> num <* char ',') (num <* char ',') num
  eof
  return $ p:ps


data Axis = X | Y | Z

axis :: Axis -> (a,a,a) -> a
axis X (x,_,_) = x
axis Y (_,y,_) = y
axis Z (_,_,z) = z

rotate :: Axis -> Axis
rotate X = Y
rotate Y = Z
rotate Z = X
