{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Control.Applicative
import Control.Monad.State
import Data.Char

newtype Parser a = Parser {runParser :: StateT String Maybe a} deriving (Functor, Applicative, Monad, Alternative)

char :: Char -> Parser Char
char c = Parser $ get >>= parseChar where
    parseChar [] = empty
    parseChar (x:xs) | x == c    = put xs >> return x
                     | otherwise = empty

digit :: Num a =>  Parser a
digit = Parser $ get >>= parseDigit where
    parseDigit [] = empty
    parseDigit (x:xs) | isDigit x = put xs >> return (fromInteger $ read [x])
                      | otherwise = empty

num :: Num a => Parser a
num = acc <$> some digit where
    acc = foldl' ((+) . (* 10)) 0

eof :: Parser ()
eof = Parser $ get >>= \case [] -> return ()
                             otherwise -> empty

notEof :: Parser ()
notEof = Parser $ get >>= \case [] -> empty
                                otherwise -> return ()

str :: String -> Parser ()
str s = sequence_ $ char <$> s
