module Main where
import Parser
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad.State (evalStateT)

main :: IO ()
main = do
  content <- readFile "aoc2025/input"
  let (shapes, regions) = fromJust $ evalStateT (runParser parseInput) (content ++ "\n")
  print $ fmap (length . filter id) $ sequence $ fmap (\(xy, ns) -> check xy (zip shapes ns)) regions

type Shape = [Char]

sizeOf :: Shape -> Int
sizeOf = length . filter (=='#')

check :: (Int, Int) -> [(Shape, Int)] -> Maybe Bool
check (x,y) shapes
  | totalSize > area = Just False
  | (div x 3) * (div y 3) >= sum (snd <$> shapes) = Just True
  | otherwise = Nothing
 where
  totalSize = sum $ fmap (\(s, n) -> sizeOf s * n) shapes
  area = x * y

parseInput :: Parser ([Shape], [((Int, Int), [Int])])
parseInput = do
  shapes <- some parseShape
  regions <- some parseRegion
  return (shapes, regions)
 where
  parseShape = num >> str ":\n" >> do
    cs1 <- parseLine
    cs2 <- parseLine
    cs3 <- parseLine
    _ <- char '\n'
    return $ cs1 ++ cs2 ++ cs3
   where
    parseLine = do
      c1 <- char '#' <|> char '.'
      c2 <- char '#' <|> char '.'
      c3 <- char '#' <|> char '.'
      _ <- char '\n'
      return [c1, c2, c3]
  parseRegion = do
    x <- num
    _ <- char 'x'
    y <- num
    _ <- char ':'
    ns <- some $ char ' ' *> num
    _ <- char '\n'
    return ((x,y), ns)
