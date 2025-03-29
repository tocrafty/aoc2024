{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Array.ST
import Control.Monad.State
import Control.Monad.ST (ST, runST)
import Control.Applicative
import Parser
import GHC.Arr
import GHC.Data.Maybe
import Control.Lens
import Control.Arrow
import Debug.Trace (trace)
import Data.Foldable
import Data.Ord (comparing)
import qualified Data.Set as S

data Traces = Traces
  { _west  :: Maybe Trace
  , _east  :: Maybe Trace
  , _north :: Maybe Trace
  , _south :: Maybe Trace
  } deriving Show

data Trace = Trace
  { prevs :: [Prev]
  , score :: Int
  } deriving Show

data Prev = Back Dir | Inplace Dir | Nil deriving Show

emptyTraces :: Traces
emptyTraces = Traces
  { _west = Nothing
  , _east = Nothing
  , _north = Nothing
  , _south = Nothing
  }

data Ceil = Wall
          | Ceil Traces
          | Start
          | End Traces
          deriving Show

type Pos = (Int, Int)

data Dir = West | East | North | South deriving Show

turnLft :: Dir -> Dir
turnLft West = South
turnLft South = East
turnLft East = North
turnLft North = West

turnRht :: Dir -> Dir
turnRht = turnLft . turnLft . turnLft

turnBack :: Dir -> Dir
turnBack = turnLft . turnLft

forward :: Pos -> Dir -> Pos
forward (x,y) West = (x, y-1)
forward (x,y) East = (x, y+1)
forward (x,y) North = (x-1, y)
forward (x,y) South = (x+1, y)

type Map s =  STArray s Pos Ceil

parseMap :: Parser [[Ceil]]
parseMap = do
  ceils <- many $ some parseCeil <* char '\n'
  lastLine <- some parseCeil <* eof
  return $ ceils ++ [lastLine]
 where
  parseCeil :: Parser Ceil
  parseCeil = (Wall <$ char '#')
          <|> (Ceil emptyTraces <$ char '.')
          <|> (Start <$ char 'S')
          <|> (End emptyTraces <$ char 'E')

findPos :: [[a]] -> (a -> Bool) -> Maybe Pos
findPos [] _ = Nothing
findPos (as:ass) f = case findPos' as f of
  Nothing -> first (+1) <$> findPos ass f
  Just y -> Just (0, y)
 where
  findPos' :: [a] -> (a -> Bool) -> Maybe Int
  findPos' [] _ = Nothing
  findPos' (a:as) f = if f a then Just 0 else (1 +) <$> findPos' as f

makeLenses ''Traces

dirLens :: Dir -> Lens' Traces (Maybe Trace)
dirLens West = west
dirLens East = east
dirLens North = north
dirLens South = south

dye :: Pos -> Dir -> Prev -> Int -> MaybeT (StateT (Map s) (ST s)) ()
dye pos dir prev sc = do
  map <- get
  ceil <- lift $ lift $ readSTArray map pos
  -- trace (show (pos, ceil)) $ return ()
  let modifyTrace' cons traces tr = lift $ lift $ writeSTArray map pos (cons $ traces & dirLens dir .~ Just tr)
      modifyTrace cons traces = case traces ^. dirLens dir of
        Nothing -> modifyTrace' cons traces Trace{prevs = [prev], score = sc}
        Just tr -> if sc < score tr
                 then modifyTrace' cons traces Trace{prevs = [prev], score = sc}
                 else if sc == score tr
                 then do
                   -- save path only, then prune this branch immediately
                   modifyTrace' cons traces $ tr{prevs = prev : prevs tr}
                   MaybeT $ return Nothing
                 else MaybeT $ return Nothing
  _ <- case ceil of
    Wall -> MaybeT $ return Nothing
    Start -> modifyTrace Ceil emptyTraces
    Ceil score -> modifyTrace Ceil score
    End score -> modifyTrace End score
  runAll
    [ dye (forward pos dir) dir (Back $ turnBack dir) (sc+1)
    , dye pos (turnLft dir) (Inplace dir) (sc+1000)
    , dye pos (turnBack dir) (Inplace dir) (sc+2000)
    , dye pos (turnRht dir) (Inplace dir) (sc+1000)
    ]
 where
  runAll :: Monad m => [MaybeT m ()] -> MaybeT m ()
  runAll ms = MaybeT $ Just <$> (sequence_ $ runMaybeT <$> ms)

main :: IO ()
main = do
  contents <- readFile "./exe/Day16/input"
  let ceils = fromJust $ evalStateT (runParser parseMap) contents
      startPos = fromJust $ findPos ceils (\case Start -> True; _ -> False)
      endPos = fromJust $ findPos ceils (\case (End _) -> True; _ -> False)
  print $ runST $ do
    map <- newListArray ((0, 0), (length ceils - 1, length (head ceils) - 1)) $ concat ceils
    _ <- execStateT (runMaybeT (dye startPos East Nil 0)) map
    (End sc) <- readSTArray map endPos
    case minimumBy (comparing score) <$> (sequence $ (sc ^.) <$> [east, west, south, north]) of
      Nothing -> return Nothing
      Just tr -> do
        poss <- traceroute map endPos tr
        let ln = S.size $ S.fromList poss
        return $ Just (score tr, ln)

traceroute :: Map s -> Pos -> Trace -> ST s [Pos]
traceroute map pos = do
  fmap (pos:) . fmap concat . sequence . fmap tracePrev . prevs
 where
  tracePrev Nil = return []
  tracePrev (Back dir) = do
    let pos' = forward pos dir
    ceil <- readSTArray map $ pos'
    traceCeil pos' ceil (turnBack dir)
  tracePrev (Inplace dir) = do
    ceil <- readSTArray map pos
    traceCeil pos ceil dir
  traceCeil pos ceil dir = case ceil of
    Wall -> error "tracing a Wall"
    Start -> return []
    Ceil traces -> traceTraces pos traces dir
    End traces -> traceTraces pos traces dir
  traceTraces pos traces dir = case traces ^. dirLens dir of
      Nothing -> error "impossible unreached ceil dir"
      Just trace -> traceroute map pos trace

instance MonadFail (ST s) where
  fail msg = error msg
