module Day1 where

import Data.List (sort)

main :: IO ()
main = do
    content <- readFile "./exe/Day1/input"
    let nss :: [[Integer]] = fmap read . words <$> lines content
    putStrLn $ ("part-1: " ++)
             $ show $ sum
             $ fmap (\(x:y:_) -> abs $ x - y)
             . transpose
             $ fmap sort
             . transpose
             $ nss
    let columns2 = concat $ fmap tail nss
    putStrLn $ ("part-2: " ++)
             $ show $ sum
             $ fmap (\(a,b) -> a*b)
             $ fmap (\(x:_) -> (x, toInteger
                                 $ length
                                 $ filter (x ==) columns2)
                    ) nss

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)
