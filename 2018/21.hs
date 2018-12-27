#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators


import           Util.Util

type Mem = Vector Int

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri | Gtrr
            | Eqir | Eqri | Eqrr
  deriving (Show, Eq, Ord, Read, Enum)

data Op = Op Opcode Int Int Int
  deriving (Show, Eq, Ord)

stepMem :: Op -> Mem -> Mem
stepMem (Op op a b c) mem = case op of
  Addr -> set c (reg a + reg b)
  Addi -> set c (reg a + b)
  Mulr -> set c (reg a * reg b)
  Muli -> set c (reg a * b)
  Banr -> set c (reg a .&. reg b)
  Bani -> set c (reg a .&. b)
  Borr -> set c (reg a .|. reg b)
  Bori -> set c (reg a .|. b)
  Setr -> set c (reg a)
  Seti -> set c (a)
  Gtir -> set c (if a > reg b then 1 else 0)
  Gtri -> set c (if reg a > b then 1 else 0)
  Gtrr -> set c (if reg a > reg b then 1 else 0)
  Eqir -> set c (if a == reg b then 1 else 0)
  Eqri -> set c (if reg a == b then 1 else 0)
  Eqrr -> set c (if reg a == reg b then 1 else 0)
  where
    reg i
      | i >= 0 && i < V.length mem = mem V.! i
      | otherwise = error "out of bounds"
    set i x
      | i >= 0 && i < V.length mem = V.take i mem <> V.singleton x <> V.drop (i+1) mem
      | otherwise = error "out of bounds"

data VM = VM {
  vmMem :: Mem
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable VM where
  hashWithSalt s (VM mem) = hashWithSalt s (V.toList mem)

data Program = Program {
  progIpp :: Int,
  progCode :: Vector Op
  }
  deriving (Show, Eq)

step :: Program -> VM -> VM
step (Program ipp prog) (VM mem)
  | inRange = let mem' = stepMem (prog V.! ip) mem
                  ip' = mem' V.! ipp + 1
                  mem'' = mem' V.// [(ipp, ip')]
              in t (VM mem'')
  | otherwise = VM mem
  where
    ip = mem V.! ipp
    inRange = ip >= 0 && ip < V.length prog
    t = if prog V.! ip == Op Eqrr 4 0 5 then
          traceShow mem
        else id

run :: HashSet VM -> Program -> VM -> VM
run seen prog vm
  | progCode prog V.! ip == Op Eqrr 4 0 5 =
    if HashSet.member vm' seen then
      vm
    else
      run (HashSet.insert vm seen) prog vm'
  | otherwise = run seen prog vm'
  where
    ip = vmMem vm V.! progIpp prog
    vm' = step prog vm

pInput :: (Monad m, CharParsing m) => m Program
pInput = Program <$> pIpp <*> (V.fromList <$> some pOp)
  where
    pIpp = string "#ip" *> spaces *> number <* spaces
    pOp = (Op
           <$> (pOpcode <* spaces)
           <*> (number <* spaces)
           <*> (number <* spaces)
           <*> (number <* spaces))
    pOpcode = read . over _head toUpper <$> some letter

main :: IO ()
main = do
  txt <- readFile "input/21.txt"
  let input = parse pInput txt
  print input

  print (run HashSet.empty input (VM $ V.replicate 6 0))

-- #ip 1
-- seti 123 0 4
-- bani 4 456 4
-- eqri 4 72 4
-- addr 4 1 1
-- seti 0 0 1
-- seti 0 2 4
-- bori 4 65536 3     <- #6
-- seti 10552971 1 4
-- bani 3 255 5       <- #8
-- addr 4 5 4
-- bani 4 16777215 4
-- muli 4 65899 4
-- bani 4 16777215 4
-- gtir 256 3 5 \
-- addr 5 1 1   | if 256 > [3] then goto #28
-- addi 1 1 1   |
-- seti 27 7 1  /
-- seti 0 1 5
-- addi 5 1 2   <- #18
-- muli 2 256 2
-- gtrr 2 3 2   \
-- addr 2 1 1   | if [2] > [3] then goto #26
-- addi 1 1 1   |
-- seti 25 0 1  /
-- addi 5 1 5
-- seti 17 2 1  -> #18
-- setr 5 7 3 <- #26
-- seti 7 8 1   -> #8
-- eqrr 4 0 5 <- #28  \
-- addr 5 1 1         | if [4] == [0] then halt, else goto #6
-- seti 5 0 1 -> #6   /
