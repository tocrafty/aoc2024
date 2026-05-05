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

main :: IO ()
main = do
  content <- readFile "aoc2025/input"
  let machines = fromJust $ evalStateT (runParser parseInput) content
  --print $ sum $ (\(target, ts, _) -> minToggles (fmap replToggle ts) target) <$> machines
  --print $ sum $ fmap (\(_, tss, js) -> trace ("tss: " ++ show tss ++ "js: " ++ show js) $ runST $ do
  --  let end = length js - 1
  --  ht <- HT.new
  --  HT.insert ht (A.listArray (0, end) $ take (end+1) $ repeat 0) 0
  --  bfs ht (A.listArray (0, end) js) tss 1
  --  ) $ machines
  print $ sum $ catMaybes $ fmap (\(_, tss, js) -> trace (show tss ++ " " ++ show js) $ minToggle tss (list2Array js)) $ machines
 where
  list2Array xs = A.listArray (0, length xs - 1) xs

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

minToggle :: [[Int]] -> A.Array Int Int -> Maybe Int
minToggle tss' target =
  -- trace ("minToggle: " ++ show tss ++ " --- " ++ show (A.elems target)) $
  if any (<0) target then Nothing
  else if all (==0) target then Just 0
  else if length toggles == 0 then Nothing
  else ((A.!) target b +) . minimum <$> case catMaybes (minToggle' <$> allCases) of
    [] -> Nothing
    x -> Just x
 where
  zeros = filter ((== 0) . (A.!) target) (A.indices target)
  tss = filter (not . flip containsAny zeros) tss'
  containsAny xs as = any id $ fmap ($ xs) $ elem <$> as
  (b, toggles) = minimumBy (comparing (\(x, bs) -> (length bs, (A.!) target x))) $ fmap (\x -> (x, toggleWithButton x)) (filter ((/= 0) . (A.!) target) (A.indices target))
  toggleWithButton x = filter (elem x) tss
  toggleBounds = (minimum . fmap ((A.!) target)) <$> toggles
  allCases = zip toggles <$> distribute' ((A.!) target b) toggleBounds
  reduceJoltage j ts n = (A.//) j $ (\x -> (x, (A.!) j x - n)) <$> ts
  reduceJoltages = foldl (\j (ts, n) -> reduceJoltage j ts n) target
  minToggle' jns =
    (if b == 7 then trace ("tss: " ++ show tss ++ " target: " ++ show (A.elems target) ++ " b: " ++ show b ++ " jns: " ++ show jns) else id) $
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
  | otherwise = trace ("joltages: " ++ show joltages) $ do
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
