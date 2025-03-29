{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Array as A
import Control.Monad.State
import Data.Bits (Bits(xor))
import Parser (Parser (runParser), char, eof, str, num)
import Control.Applicative
import Data.Maybe
import Data.List (intercalate, isPrefixOf)
import Control.Arrow
import Debug.Trace
import GHC.Generics
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.RWS

main :: IO ()
main = do
  contents <- readFile "exe/Day17/input"
  let com = fromJust $ evalStateT (runParser parseCom) contents
  print com
  print $ intercalate ","
        $ reverse
        $ fmap show
        $ outs
        $ flip execState com
        $ runProg
  let prog' = reverse $ tail $ reverse $ A.elems $ prog com
  print $ head $ runReader (allChoices (progToOuts $ A.elems $ prog com)) (A.listArray (0, length prog' - 1) prog')
 where
  progToOuts [] = []
  progToOuts (Instru opCode operand : intrus) = [opCode, operand] <> progToOuts intrus
  
type Reg = Int

data Regs = Regs
  { regA :: Reg
  , regB :: Reg
  , regC :: Reg
  } deriving (Show, Eq, Generic)

data Computer = Computer
  { regs :: Regs
  , outs :: [Int]
  , prog :: A.Array Int Instru
  , ptr  :: Int
  } deriving (Show, Eq)

data Instru = Instru OpCode Operand deriving (Show, Eq)

type OpCode = Int

type Operand = Int

combo :: Regs -> Operand -> Int
combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo regs 4 = regA regs
combo regs 5 = regB regs
combo regs 6 = regC regs
combo _ x = error $ "invalid combo operand" <> show x

literal :: Operand -> Int
literal = id

type ProgPtr = Int

runInstru :: Instru -> State Computer ()
runInstru (Instru 0 op) = do
  com <- get
  let a = regA $ regs $ com
      power = combo (regs com) op
      a' = a `div` 2 ^ power
  put com { regs = (regs com) { regA = a' }
          , ptr = (ptr com) + 1 }
runInstru (Instru 1 op) = do
  com <- get
  let b = regB $ regs $ com
      b' = b `xor` literal op
  put com { regs = (regs com) { regB = b' }
          , ptr = (ptr com) + 1 }
runInstru (Instru 2 op) = do
  com <- get
  let x = combo (regs com) op
      b = x `mod` 8
  put com { regs = (regs com) { regB = b }
          , ptr = (ptr com) + 1 }
runInstru (Instru 3 op) = do
  com <- get
  if regA (regs com) == 0
  then put com { ptr = (ptr com) + 1 }
  else put com { ptr = literal op `div` 2 }
runInstru (Instru 4 _) = do
  com <- get
  let b = regB $ regs com
      c = regC $ regs com
      b' = b `xor` c
  put com { regs = (regs com) { regB = b' }
          , ptr = (ptr com) + 1 }
runInstru (Instru 5 op) = do
  com <- get
  let x = combo (regs com) op `mod` 8
  put com { outs = x : (outs com)
          , ptr = (ptr com) + 1 }
runInstru (Instru 6 op) = do
  com <- get
  let a = regA $ regs $ com
      power = combo (regs com) op
      b = a `div` 2 ^ power
  put com { regs = (regs com) { regB = b }
          , ptr = (ptr com) + 1 }
runInstru (Instru 7 op) = do
  com <- get
  let a = regA $ regs $ com
      power = combo (regs com) op
      c = a `div` 2 ^ power
  put com { regs = (regs com) { regC = c }
          , ptr = (ptr com) + 1 }
runInstru (Instru opCode _) = error $ "invalid op code " <> show opCode

runProg :: State Computer ()
runProg = do
  com <- get
  let (lower, upper) = A.bounds (prog com)
  if (ptr com) < lower || (ptr com) > upper
  then return ()
  else do
    put $ execState (runInstru $ (prog com) A.! (ptr com)) com
    runProg

parseCom :: Parser Computer
parseCom = do
  regs <- parseRegs
  _ <- char '\n'
  prog <- parseProg
  eof
  return Computer { regs = regs
                  , outs = []
                  , prog = prog
                  , ptr = 0}
 where
  parseRegs = do
    regA <- parseReg 'A'
    regB <- parseReg 'B'
    regC <- parseReg 'C'
    return $ Regs regA regB regC
   where
    parseReg x = str "Register " *> char x *> str ": " *> num <* char '\n'
  parseProg = do
    _ <- str "Program: "
    instrus' <- some (parseInstru <* char ',')
    lastInstru <- parseInstru <* eof
    let instrus = instrus' <> [lastInstru]
    return $ A.listArray (0, length instrus - 1) instrus
   where
    parseInstru = (Instru <$> num <* char ',') <*> num

-- this is a special method for my own input only.
climeUp :: Int -> [Int] -> Reader (A.Array Int Instru) [Int]
climeUp out as = do
  instrus <- ask
  let as' = (+) <$> fmap (*8) as <*> [0..7]
      coms = [(a, Computer { regs = Regs { regA = a
                                         , regB = 0
                                         , regC = 0 }
                           , outs = []
                           , prog = instrus
                           , ptr = 0 })
              | a <- as']
      coms' = second (execState runProg) <$> coms
  return $ fmap fst $ filter (isPrefixOf [out] . outs . snd) coms'

allChoices :: [Int] -> Reader (A.Array Int Instru) [Int]
allChoices = foldr (flip (>>=)) (return [0]) . fmap climeUp
