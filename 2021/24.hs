{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.Codensity
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Debug.Trace
import qualified Ersatz as E
import qualified Ersatz.Counting as E
import qualified Ersatz.Solver.Minisat as E
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Numeric.Search.Range
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           AStar
import           Util
-- import qualified Util.Text as T

data V = Var Char | Imm Int
  deriving (Show, Eq, Ord)
data Inst = Inp Char | Add Char V | Mul Char V | Div Char V | Mod Char V | Eql Char V
  deriving (Show, Eq, Ord)

parseInput :: Parser [Inst]
parseInput = some $ asum [
  do text "inp "; a <- letter; spaces ; return (Inp a),
  do text "add "; a <- letter; char ' '; b <- (Var <$> letter) <|> (Imm <$> p_int) ; spaces ; return (Add a b),
  do text "mul "; a <- letter; char ' '; b <- (Var <$> letter) <|> (Imm <$> p_int) ; spaces ; return (Mul a b),
  do text "div "; a <- letter; char ' '; b <- (Var <$> letter) <|> (Imm <$> p_int) ; spaces ; return (Div a b),
  do text "mod "; a <- letter; char ' '; b <- (Var <$> letter) <|> (Imm <$> p_int) ; spaces ; return (Mod a b),
  do text "eql "; a <- letter; char ' '; b <- (Var <$> letter) <|> (Imm <$> p_int) ; spaces ; return (Eql a b)
  ]

exec :: [Inst] -> [Int] -> Map Char Int
exec = go (Map.fromList [(v,0) | v <- "xyzw"])
  where
    go state [] _ = state
    go state (Inp v:ops) (w:ws) = go (Map.insert v w state) ops ws
    go state (op:ops) inputs = go state' ops inputs
      where
        state' = case op of
          Add t val -> Map.insert t (state Map.! t + value val) state
          Mul t val -> Map.insert t (state Map.! t * value val) state
          Div t val -> Map.insert t (state Map.! t `div` value val) state
          Mod t val -> Map.insert t (state Map.! t `mod` value val) state
          Eql t val -> Map.insert t (if value val == state Map.! t then 1 else 0) state
        value (Imm i) = i
        value (Var v) = state Map.! v

data Bit64 = Bit64 E.Bits
  deriving (Show)

truncateBits :: E.Bits -> E.Bits
truncateBits (E.Bits bs) = E.Bits (take 64 bs)

isnonneg :: Bit64 -> E.Bit
isnonneg (Bit64 bs) = bs E.<? 2^63

ispos :: Bit64 -> E.Bit
ispos (Bit64 bs) = bs E./== 0 E.&& bs E.<? 2^63

isnonpos :: Bit64 -> E.Bit
isnonpos a = E.not (ispos a)

isneg :: Bit64 -> E.Bit
isneg (Bit64 bs) = bs E.>=? 2^63

instance Num Bit64 where
  fromInteger n | n >= 0 = Bit64 (truncateBits (E.encode n))
                | n < 0  = negate (fromInteger (abs n))
  (+) (Bit64 a) (Bit64 b) = Bit64 (truncateBits (a + b))
  (-) a b = a + negate b
  negate (Bit64 a) = Bit64 (truncateBits $ 2^64 - a)
  (*) (Bit64 a) (Bit64 b) = Bit64 (truncateBits (a * b))

instance E.Variable Bit64 where
  literally f = Bit64 . E.Bits <$> replicateM 64 (E.literally f)

instance E.Equatable Bit64 where
  Bit64 a === Bit64 b = a E.=== b
  Bit64 a /== Bit64 b = a E./== b

instance E.Codec Bit64 where
  type Decoded Bit64 = Int
  decode sol (Bit64 b) = fromIntegral <$> E.decode sol b
  encode n = fromIntegral n

abs64 :: Bit64 -> Ersatz E.Bits
abs64 a = do
  absa <- E.exists
  E.assert (isnonneg a E.==> absa E.=== a)
  E.assert (isneg a E.==> absa E.=== negate a)
  return (case absa of Bit64 bs -> bs)

divModBit64 :: Bit64 -> Bit64 -> Ersatz (Bit64, Bit64)
divModBit64 a b = do
  div <- E.exists
  mod <- E.exists
  let val = b * div + mod
  absa <- abs64 a
  absb <- abs64 b
  absdiv <- abs64 div
  absbdiv <- abs64 (b * div)
  absmod <- abs64 mod
  E.assert (val E.=== a)
  E.assert (isnonneg (a*b) E.==> isnonneg mod E.&&
            isnonpos (a*b) E.==> isnonpos mod E.&&
            isnonneg (a*b) E.==> isnonneg div E.&&
            isnonpos (a*b) E.==> isnonpos div)
  E.assert (absmod E.<? absb)
  E.assert (absdiv E.<=? absa)
  E.assert (absbdiv E.<=? absa)
  return (div, a - b * div)

divBit64 :: Bit64 -> Bit64 -> Ersatz Bit64
divBit64 a b = fst <$> divModBit64 a b

modBit64 :: Bit64 -> Bit64 -> Ersatz Bit64
modBit64 a b = snd <$> divModBit64 a b

execE :: [Inst] -> [Bit64] -> Ersatz (Map Char Bit64)
execE = go (Map.fromList [(v,0) | v <- "xyzw"])
  where
    go state [] _ = pure state
    go state (Inp v:ops) (w:ws) = go (Map.insert v w state) ops ws
    go state (op:ops) inputs = do
      state' <- case op of
        Add t val -> return $ update t (state Map.! t + value val)
        Mul t val -> return $ update t (state Map.! t * value val)
        Div t val -> update t <$> (state Map.! t `divBit64` value val)
        Mod t val -> update t <$> (state Map.! t `modBit64` value val)
        Eql t val -> return $ update t (Bit64 $ E.bits $ value val E.=== state Map.! t)
      go state' ops inputs
        where
          value (Imm i) = E.encode (fromIntegral i)
          value (Var v) = state Map.! v
          update t val = Map.insert t val state


input = unsafePerformIO (parse parseInput <$> T.readFile "input/24.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/24_sample.txt")

gtx :: Bit64 -> Bit64 -> E.Bit
gtx (Bit64 a) (Bit64 b) = a E.>? b

ltx :: Bit64 -> Bit64 -> E.Bit
ltx (Bit64 a) (Bit64 b) = a E.<? b

gt_sequence :: [Bit64] -> [Bit64] -> E.Bit
gt_sequence [] [] = E.false
gt_sequence (a:as) (b:bs) = ((a E.=== b) E.&& gt_sequence as bs) E.|| gtx a b

lt_sequence :: [Bit64] -> [Bit64] -> E.Bit
lt_sequence [] [] = E.false
lt_sequence (a:as) (b:bs) = ((a E.=== b) E.&& lt_sequence as bs) E.|| ltx a b

part1 input = go [4,9,9,1,1,3,1,6,8,2,1,8,9,9]
  where
    go seq = case solve1 (puzzle seq) of
      [seq'] -> traceShow seq' (go seq')
      [] -> seq
    puzzle seq = do
      inputs <- replicateM 14 $ do a <- E.exists
                                   E.assert $ let (Bit64 bs) = a in bs E.>=? 1 E.&& bs E.<=? 9
                                   return a
      E.assert $ gt_sequence inputs (map E.encode seq)
      ans <- execE input inputs
      E.assert (ans Map.! 'z' E.=== 0)
      return inputs

part2 input = go [4,9,9,1,1,3,1,6,8,2,1,8,9,9]
  where
    go seq = case solve1 (puzzle seq) of
      [seq'] -> traceShow seq' (go seq')
      [] -> seq
    puzzle seq = do
      inputs <- replicateM 14 $ do a <- E.exists
                                   E.assert $ let (Bit64 bs) = a in bs E.>=? 1 E.&& bs E.<=? 9
                                   return a
      E.assert $ lt_sequence inputs (map E.encode seq)
      ans <- execE input inputs
      E.assert (ans Map.! 'z' E.=== 0)
      return inputs