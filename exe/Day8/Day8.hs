module Main where


import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Array
import qualified Data.HashSet as HS
import Data.List (tails)

main :: IO ()
main = do
    contents <- lines <$> readFile "./exe/Day8/input"
    let mp = listArray ((1,1), (length contents, length $ head contents)) (concat contents)
        boundaries = bounds mp
    print $ flip numOfAntiNodes mp $ \x y -> maybe [] return (resonant boundaries x y)
                                     ++ maybe [] return (resonant boundaries y x)
    print $ flip numOfAntiNodes mp $ \x y -> [x,y]
                                     ++ resonants boundaries x y
                                     ++ resonants boundaries y x

numOfAntiNodes :: (Pos -> Pos -> [Pos]) -> Array Pos Char -> Int
numOfAntiNodes antiNodes =
  HS.size
    . HS.fromList
    . HM.foldl (<>) []
    . fmap (concatMap (uncurry antiNodes) . combination2)
    . groupByFrequency

type Pos = (Int, Int)

resonant :: (Pos, Pos) -> Pos -> Pos -> Maybe Pos
resonant r (x1,y1) (x2,y2) = if inRange r xy then Just xy else Nothing where
    xy = (2*x2-x1, 2*y2-y1)

resonants :: (Pos, Pos) -> Pos -> Pos -> [Pos]
resonants r p1 p2 = maybe [] (\p -> p:(resonants r p2 p)) (resonant r p1 p2)

combination2 :: [x] -> [(x,x)]
combination2 xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

groupByFrequency :: Array Pos Char -> HM.HashMap Char [Pos]
groupByFrequency = foldl insert HM.empty . assocs where
    insert acc (pos, c) | isAlpha c || isDigit c = HM.insertWith (<>) c [pos] acc
                        | otherwise = acc
