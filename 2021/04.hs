{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
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
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
-- import qualified Util.Text as T

parseInput :: Parser ([Int], [[[Int]]])
parseInput = do
  nums <- some (p_nat <* optional (char ','))
  spaces
  boards <- some $ do
    replicateM 5 (replicateM 5 (p_nat <* spaces))
  return (nums, boards)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/04.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/04_sample.txt")

part1 input = go Set.empty nums
  where
    (nums, boards) = input
    iswin set board = any (all (`Set.member` set)) board || any (all (`Set.member` set)) (transpose board)
    go set (n:ns)
      | any (iswin set') boards = [score set' b n | b <- boards, iswin set' b]!!0
      | otherwise = go set' ns
      where
        set' = Set.insert n set
    score set board n = n * sum (filter (\k -> not (Set.member k set)) (fold board))

part2 input = go Set.empty nums boards
  where
    (nums, boards) = input
    iswin set board = any (all (`Set.member` set)) board || any (all (`Set.member` set)) (transpose board)
    go set (n:ns) boards
      | [b] <- boards, iswin set' b = score set' b n
      | otherwise = go set' ns (filter (not . iswin set') boards)
      where
        set' = Set.insert n set
    score set board n = n * sum (filter (\k -> not (Set.member k set)) (fold board))
