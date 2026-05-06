module Main where
import Parser
import Control.Applicative
import Control.Monad.State (evalStateT)
import Data.Maybe
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST
import Data.Bits
import Data.Foldable
import qualified Data.Array as A
import Debug.Trace
import Data.Hashable
import Data.Ord
import qualified Data.Array.ST as MA

trace' = const id

main :: IO ()
main = do
  content <- readFile "aoc2025/input"
  let machines = fromJust $ evalStateT (runParser parseInput) content
  print $ sum $ (\(target, ts, _) -> minToggles (fmap replToggle ts) target) <$> machines
  --print $ fmap (\(_, tss, js) -> minToggle tss (list2Array js)) $ machines
  print $ sum $ catMaybes $ fmap (
    \(_, toggles, joltages) -> runST $ do
      trace' (show joltages) $ return ()
      (p2ts, cache) <- parity2Combs toggles
      t0s <- MA.readArray p2ts 0
      minToggleByParity p2ts cache (list2Array joltages) (concat <$> ([]:allCombs (filter (not.null) t0s)))
    ) $ machines
 where
  list2Array xs = A.listArray (0, length xs - 1) xs

-- another method is math way. Solve free matrix first and get the result.

-- see https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
minToggleByParity :: MA.STArray s Int [[Toggle]] -> HT.HashTable s (A.Array Int Int) (Maybe Int) -> A.Array Int Int -> [[Toggle]] -> ST s (Maybe Int)
minToggleByParity p2ts cache joltages combsOf0 = do
  n' <- HT.lookup cache joltages
  case n' of
    Just n -> return n
    _ -> let pj = parityOf joltages in
      if pj == 0 then do
        minToggles <- sequence $ fmap (
          \ts -> do
            let js = remain ts
            if (any (<0) js) then trace' ("invalid: " ++ show (A.elems js) ++ " tss: " ++ show ts) $ return Nothing
            else do
              r <- minToggleByParity p2ts cache (fmap (`div` 2) js) combsOf0
              trace' ("valid: " ++ show (A.elems $ fmap (`div` 2) js) ++ " tss: " ++ show ts ++ " r: " ++ show r) $ return ()
              return $ fmap ((+ length ts) . (* 2)) r
          ) combsOf0
        case catMaybes minToggles of
          [] -> do
            trace' ("invalid: " ++ show (A.elems joltages)) $ HT.insert cache joltages Nothing
            return Nothing
          xs -> do
            let r = minimum xs
            trace' ("joltages: " ++ show (A.elems joltages) ++ " pj: " ++ show pj ++ " pj == 0: " ++ " min: " ++ show r) $
              HT.insert cache joltages (Just r)
            return $ Just r
      else do
        tss <- MA.readArray p2ts pj
        minToggles <- sequence $ fmap (
          \ts -> let js = remain ts in if (any (<0) js) then return Nothing else do
            r <- minToggleByParity p2ts cache js combsOf0
            return $ (length ts + ) <$> r
          ) tss
        case catMaybes minToggles of
          [] -> do
            trace' ("invalid: " ++ show (A.elems joltages)) $ HT.insert cache joltages Nothing
            return Nothing
          xs -> do
            let r = minimum xs
            trace' ("joltages: " ++ show (A.elems joltages) ++ " pj: " ++ show pj ++ " ts: " ++ show tss ++ " min: " ++ show r) $
              HT.insert cache joltages (Just r)
            return $ Just r
 where
  minus t toggle = (A.//) t $ fmap (\i -> (i, (A.!) t i - 1)) toggle
  remain toggles = foldl' (minus) joltages toggles

parityOf :: Integral a => A.Array i a -> a
parityOf joltages = sum $ uncurry (*) <$> zip ((2^) <$> [0..]) ((`mod` 2) <$> A.elems joltages)

type Toggle = [Int]

parity2Combs :: [Toggle] -> ST s (MA.STArray s Int [[Toggle]], HT.HashTable s (A.Array Int Int) (Maybe Int))
parity2Combs toggles = do
  p2ts <- MA.newArray (0, maxParity) []
  cache <- HT.new
  HT.insert cache zeros (Just 0)
  sequence_ $ fmap (\toggles -> do
    let p = parityOf (addAll toggles)
    if p == 0 then trace' ("zero: " ++ show toggles) $ return () else return ()
    MA.modifyArray p2ts p (toggles :)
    ) $ allCombs toggles
  return $ (p2ts, cache)
 where
  maxIdx = maximum (fmap maximum toggles)
  maxParity = 2 ^ (1 + maxIdx) - 1
  add joltages toggle = (A.//) joltages $ fmap (\i -> (i, (A.!) joltages i + 1)) toggle
  zeros = A.listArray (0, maxIdx) $ take (maxIdx + 1) $ repeat 0
  addAll = foldl' add zeros

allCombs :: [a] -> [[a]]
allCombs [] = [[]]
allCombs (x:xs) = allCombs xs ++ fmap (x:) (allCombs xs)

minToggles :: [Int] -> Int -> Int
minToggles ts target = runST $ do
  ht <- HT.new
  HT.insert ht 0 0
  dfs ht ts 1 0
  fromJust <$> HT.lookup ht target

dfs :: HT.HashTable s Int Int -> [Int] -> Int -> Int -> ST s ()
dfs ht ts c l = do
  traverse_ (\light -> do
    ms <- HT.lookup ht light
    case ms of
      Nothing -> HT.insert ht light c >> dfs ht ts (c+1) light
      Just c' -> if c < c' then HT.insert ht light c >> dfs ht ts (c+1) light
                 else return ()
    ) $ (l `xor`) <$> ts

-- it's still very slow with many prune method. 
minToggle :: [[Int]] -> A.Array Int Int -> Maybe Int
minToggle tss' target =
  if any (<0) target then Nothing
  else if all (==0) target then Just 0
  else if length toggles == 0 then Nothing
  else ((A.!) target b +) . minimum <$> case catMaybes (minToggle' <$> allCases) of
    [] -> Nothing
    x -> Just x
 where
  zeros = filter ((== 0) . (A.!) target) (A.indices target)
  tss = filter (not . flip containsAny zeros) tss'
  containsAny xs as = any (`elem` xs) as
  (b, toggles) = minimumBy (comparing (\(x, bs) -> (length bs, (A.!) target x))) $ fmap (\x -> (x, toggleWithButton x)) (filter ((/= 0) . (A.!) target) (A.indices target))
  toggleWithButton x = filter (elem x) tss
  toggleBounds = (minimum . fmap ((A.!) target)) <$> toggles
  allCases = zip toggles <$> distribute' ((A.!) target b) toggleBounds
  reduceJoltage j ts n = (A.//) j $ (\x -> (x, (A.!) j x - n)) <$> ts
  reduceJoltages = foldl' (\j (ts, n) -> reduceJoltage j ts n) target
  minToggle' jns =
    (if b == -1 then trace' ("tss: " ++ show tss ++ " target: " ++ show (A.elems target) ++ " b: " ++ show b ++ " jns: " ++ show jns) else id) $
    minToggle tss $ reduceJoltages jns

distribute' :: Int -> [Int] -> [[Int]]
distribute' _ [] = undefined
distribute' n [m] = if n <= m then [[n]] else []
distribute' n (m:ms) = do
  n' <- [0 .. min n m]
  ns' <- distribute' (n-n') ms
  return $ n':ns'


distribute :: Int -> Int -> [[Int]]
distribute n 1 = [[n]]
distribute n k = do
  m <- [0..n]
  ms <- distribute (n-m) (k-1)
  return $ m:ms

bfs :: HT.HashTable s (A.Array Int Int) Int -> A.Array Int Int -> [[Int]] -> Int -> ST s Int
bfs ht target tss n = do
  as <- HT.foldM (\as (a, _) -> do
    return $ fmap (increase a) tss ++ as
    ) [] ht
  if any (== target) as then return n
  else do
    traverse_ (\a -> do
      current <- HT.lookup ht a
      case current of
        Nothing -> HT.insert ht a n
        _ -> return ()
      ) $ filter (< target) as
    bfs ht target tss (n+1)
 where
  increase j ts = (A.//) j $ (\i -> (i, ((A.!) j i) + 1)) <$> ts


dfs' :: HT.HashTable s (A.Array Int Int) (Maybe Int) -> A.Array Int Int -> [[Int]] -> ST s (Maybe Int)
dfs' ht joltages tss
  | any (<0) joltages = return Nothing
  | otherwise = do
    mmc <- HT.lookup ht joltages
    case mmc of
      Just mc -> return mc
      Nothing -> do
        smc <- sequence $ fmap (flip (dfs' ht) tss) $ decrease joltages <$> tss
        let mc = fmap (1+) $ fmap minimum $ atLeastOneJust $ smc
        HT.insert ht joltages mc
        return mc
       where
        decrease j ts = (A.//) j $ (\i -> (i, ((A.!) j i) - 1)) <$> ts
        atLeastOneJust mxs = case catMaybes mxs of
          [] -> Nothing
          ys -> Just ys


instance Hashable e => Hashable (A.Array Int e) where
  hashWithSalt salt arr =
    hashWithSalt salt (A.elems arr)


parseInput :: Parser [(Int, [[Int]], [Int])]
parseInput = do
  x <- (,,) <$> parseLight <*> parseToggles <*> parseJoltage
  xs <- many $ (,,) <$> (char '\n' *> parseLight) <*> parseToggles <*> parseJoltage
  return $ x:xs
 where
  parseLight :: Parser Int
  parseLight = do
    _ <- char '['
    cs <- some $ char '.' <|> char '#'
    _ <- str "] "
    return $ toLight cs (0 :: Int)
   where
    toLight [] _ = 0
    toLight (x:xs) n | x == '#' = 2 ^ n + toLight xs (n+1)
                     | otherwise = toLight xs (n+1)
  parseToggles :: Parser [[Int]]
  parseToggles = some $ do
    _ <- char '('
    n <- num
    ns <- many $ char ',' *> num
    _ <- str ") "
    return $ n : ns
    --return $ sum $ fmap (2 ^) (n:ns)
  parseJoltage :: Parser [Int]
  parseJoltage = do
    _ <- char '{'
    n <- num
    ns <- many $ char ',' *> num
    _ <- char '}'
    return $ n:ns

replToggle :: [Int] -> Int
replToggle xs = sum $ fmap (2 ^) xs
