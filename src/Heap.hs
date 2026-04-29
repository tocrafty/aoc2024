module Heap (
  newHeap,
  push,
  pop,
) where

import Control.Monad (when)
import Data.List
import Data.Ord
import Data.Primitive
import Data.Vector.Generic.Mutable (PrimMonad)
import Data.Vector.Mutable qualified as MV

newHeap :: (PrimMonad m) => m (Heap (MV.PrimState m) a)
newHeap = do
  v <- MV.new 1
  mv <- newMutVar v
  sz <- newMutVar 0
  return $ Heap mv sz

data Heap s a = Heap
  { vec :: MutVar s (MV.MVector s a)
  , size :: MutVar s Int
  }

push :: (Ord a, PrimMonad m) => Heap (MV.PrimState m) a -> a -> m ()
push h a = do
  mayExtend h
  sz <- readMutVar (size h)
  writeMutVar (size h) (sz + 1)
  v <- readMutVar (vec h)
  MV.write v sz a
  let up 0 = return ()
      up i = do
        let j = (i - 1) `div` 2
        jv <- MV.read v j
        iv <- MV.read v i
        when (jv > iv) $ MV.swap v i j >> up j
  up sz

mayExtend :: (PrimMonad m) => Heap (MV.PrimState m) a -> m ()
mayExtend h = do
  v <- readMutVar (vec h)
  sz <- readMutVar (size h)
  let cap = MV.length v
  when (sz == cap) $ do
    vec' <- MV.grow v cap
    writeMutVar (vec h) vec'

pop :: (Ord a, PrimMonad m) => Heap (MV.PrimState m) a -> m (Maybe a)
pop h = do
  v <- readMutVar (vec h)
  sz <- readMutVar (size h)
  if sz == 0 then return Nothing
  else if sz == 1 then do
    writeMutVar (size h) 0
    Just <$> MV.read v 0
  else do
    writeMutVar (size h) (sz - 1)
    MV.swap v 0 (sz - 1)
    let down i = do
          j <- fmap (fst . minimumBy (comparing snd))
             . traverse (\i -> (i,) <$> (MV.read v i))
             $ filter (< sz - 1) [i, i * 2 + 1, i * 2 + 2]
          when (j /= i) $ MV.swap v i j >> down j
    down 0
    Just <$> MV.read v (sz - 1)
