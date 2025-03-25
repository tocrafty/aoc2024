module Day2 where

main :: IO ()
main = readFile "./exe/Day2/input"
   >>= parts
     . fmap (fmap read . words)
     . lines
    where
        parts ii = sequence_
                 $ fmap (\(prompt, f) -> putStrLn
                                       $ (prompt ++)
                                       $ show
                                       $ length
                                       $ (filter id)
                                       $ fmap f ii
                        ) [("part-1: ", isSafe),
                           ("part-2: ", isSafe2)]


isSafe2 :: [Int] -> Bool
isSafe2 xs = any id $ isSafe <$> (xs: exceptOne xs) where
    exceptOne [] = []
    exceptOne (x:xs) = xs : fmap (x:) (exceptOne xs)

isSafe :: [Int] -> Bool
isSafe xs = isSafe_ $ fmap (uncurry (-)) $ zip xs (tail xs) where
    isSafe_ diffs = and (fmap ((\x -> x >=1 && x <= 3) . abs) diffs)
                     && (all (>0) diffs || all (<0) diffs)