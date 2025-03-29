{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Array.ST as A
import Control.Monad.State
import Control.Monad.ST (ST, runST)
import GHC.Arr (readSTArray, writeSTArray)
import Parser
import Control.Applicative
import Control.Exception
import Data.List
import qualified Data.Set as Set
-- import Debug.Trace
trace s = id


main :: IO ()
main = do
  contents <- readFile "./exe/Day15/input"
  let Just (wh, ch, dirs) = evalStateT (runParser parseInput) contents
  print $ runST $ do
    wh <- newListArray ((0,0), (length wh - 1, length (head wh) - 1)) $ concat wh
    (wh, _) <- execStateT (sequence_ $ move <$> dirs) (wh, ch)
    sum <$> (gps . fst <$>) <$> ((filter ((== 'O').snd)) <$> getAssocs wh)

  let wh' = expand wh
  print $ runST $ do
    wh <- newListArray ((0,0), (length wh' - 1, length (head wh') - 1)) $ concat wh'
    (wh, _) <- execStateT (sequence_ $ push <$> dirs) (wh, findCharactor wh')
    sum <$> (gps . fst <$>) <$> ((filter ((== '[').snd)) <$> getAssocs wh)
 where
  gps (x,y) = 100*x + y

data Dir = Up | Down | Lft | Rht deriving Show

type Warehouse s = A.STArray s (Int, Int) Char

type Charactor = (Int, Int)

type St s a = StateT (Warehouse s, Charactor) (ST s) a

move :: Dir -> St s ()
move dir = do
  let
    move' :: (Int, Int) -> St s (Maybe (Int, Int))
    move' pos = do
      (wh, _) <- get
      val <- lift $ readSTArray wh pos
      case val of
        '#' -> return Nothing
        'O' -> move' (next dir pos)
        _ -> return $ Just pos
  (wh, ch) <- get
  let start = next dir ch
  pos <- move' start
  case pos of
    Nothing -> return ()
    Just end -> do
      lift $ writeSTArray wh end 'O'
      lift $ writeSTArray wh start '@'
      lift $ writeSTArray wh ch '.'
      put (wh, start)

push :: forall s. Dir -> St s ()
push dir = do
  (wh, ch) <- get
  let
    start = next dir ch
    push' :: (Int, Int) -> StateT ([ST s ()], Set.Set (Int, Int)) (ST s) Bool
    push' pos = let pos' = next Rht pos in do
      c <- lift $ readSTArray wh pos
      case c of
        '[' -> do
          bs <- sequence $ push' <$> next' dir pos
          modify (\(actions, moved) -> 
            if Set.member pos moved then
              (actions, moved)
            else
              trace ("box " ++ show pos ++ " is move " ++ show dir) (
                (writeSTArray wh pos '.'
              >> writeSTArray wh pos' '.'
              >> writeSTArray wh (next dir pos) '['
              >> writeSTArray wh (next dir pos') ']'
                ) : actions, Set.insert pos moved))
          return (and bs)
        ']' -> push' (next Lft pos)
        '#' -> return False
        _ -> return True
  (ok, (actions, _)) <- lift $ runStateT (push' start) ([], Set.empty)
  if ok then do
    lift $ sequence_ $ reverse $ actions
    trace ("@" ++ show dir ++ " " ++ show start) $ lift $ writeSTArray wh start '@'
    lift $ writeSTArray wh ch '.'
    put (wh, start)
  else
    trace ("@" ++ show dir ++ " " ++ show ch ++ "unchanged") $ return ()

next :: Dir -> (Int, Int) -> (Int, Int)
next Up (x,y) = (x-1,y)
next Down (x,y) = (x+1,y)
next Lft (x,y) = (x,y-1)
next Rht (x,y) = (x,y+1)

next' :: Dir -> (Int, Int) -> [(Int, Int)]
next' Up (x,y) = [(x-1,y), (x-1,y+1)]
next' Down (x,y) = [(x+1,y), (x+1,y+1)]
next' Lft (x,y) = [(x,y-1)]
next' Rht (x,y) = [(x,y+2)]

parseInput :: Parser ([[Char]], (Int, Int), [Dir])
parseInput = do
  wh <- parseWarehouse
  _ <- char '\n'
  dirs <- parseDirs
  return (wh, findCharactor wh, dirs)

parseWarehouse :: Parser [[Char]]
parseWarehouse = some parseLine
 where
  parseLine :: Parser [Char]
  parseLine = some (char '#' <|> char '.' <|> char 'O' <|> char '@') <* char '\n'

parseDirs :: Parser [Dir]
parseDirs = do
  dss <- many (some parseDir <* char '\n')
  ds <- some parseDir
  return $ (concat dss) ++ ds

parseDir :: Parser Dir
parseDir = parse <$> (char '^' <|> char 'v' <|> char '<' <|> char '>')
 where
  parse '^' = Up
  parse 'v' = Down
  parse '<' = Lft
  parse '>' = Rht
  parse c = throw $ userError $ "invalid direction " ++ [c]


findCharactor :: [[Char]] -> (Int, Int)
findCharactor [] = throw $ userError "can not find @"
findCharactor (cs:css) =
  case elemIndex '@' cs of
    Just x -> (0, x)
    Nothing -> let (x,y) = findCharactor css in (x+1, y)

expand :: [[Char]] -> [[Char]]
expand = fmap (concat . fmap (expand')) where
  expand' 'O' = "[]"
  expand' '@' = "@."
  expand' c = [c,c]

