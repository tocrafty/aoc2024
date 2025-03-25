{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Day3 where

import Control.Monad.State
import Control.Applicative
import Data.Char
import Control.Arrow

main :: IO ()
--main = putStrLn $ show $ runStateT (runParser sumMul) (True, "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
main = readFile "./exe/Day3/input" >>= return . (True,) >>= putStrLn . show . evalStateT (runParser sumMul)

newtype Parser a = Parser {runParser :: StateT (Bool, String) Maybe a} deriving (Functor, Applicative, Monad, Alternative)

char :: Char -> Parser Char
char c = Parser $ get >>= parseChar where
    parseChar (_, []) = empty
    parseChar (b, x:xs) | x == c    = put (b, xs) >> return x
                        | otherwise = empty

digit :: Parser Integer
digit = Parser $ get >>= parseDigit where
    parseDigit (_, []) = empty
    parseDigit (b, x:xs) | isDigit x = put (b, xs) >> return (read [x])
                         | otherwise = empty

num :: Parser Integer
num = acc <$> some digit where
    acc = foldl' ((+) . (* 10)) 0

eof :: Parser ()
eof = Parser $ get >>= \case (_, []) -> return ()
                             otherwise -> empty

notEof :: Parser ()
notEof = Parser $ get >>= \case (_, []) -> empty
                                otherwise -> return ()
                            
str :: String -> Parser ()
str s = sequence_ $ char <$> s

on :: Parser ()
on = (str "do()" >> (Parser $ get >>= \(_, s) -> put (True, s))) <|> return ()

off :: Parser ()
off = (str "don't()" >> (Parser $ get >>= \(_, s) -> put (False, s))) <|> return ()

toggle :: Parser ()
toggle = on >> off

mul :: Parser Integer
mul = (*) <$> (str "mul(" *> num) <*> (char ',' *> num <* char ')')

mul' :: Parser Integer
mul' = Parser $ get >>= \case (True, _) -> runParser mul
                              (False, _) -> empty

skipOne :: Parser ()
skipOne = eof <|> Parser (get >>= put . second tail)

next :: Parser Integer -> Parser Integer
next p = toggle >> p <|> (notEof >> skipOne >> next p)

sumMul :: Parser Integer
sumMul = sum <$> many (next mul')
