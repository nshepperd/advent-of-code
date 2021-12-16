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
import           Numeric.Search.Range
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
-- import qualified Util.Text as T

type Grid a = Map (V2 Int) a

parseInput :: Parser (Grid Int)
parseInput = do
  lines <- some (some digit <* spaces)
  return $ Map.fromList [(V2 x y, read [c] :: Int)
                        |
                         (y, line) <- zip [0..] lines,
                         (x, c) <- zip [0..] line]


input = unsafePerformIO (parse parseInput <$> T.readFile "input/11.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/11_sample.txt")

neighbors p = [p + d | dx <- [-1, 0, 1], dy <- [-1, 0, 1], let d = V2 dx dy, d /= V2 0 0]

step grid = (new_grid, flashes)
  where
    new_grid = Map.fromList [(p, if Set.member p flashes then 0 else e + 1 + neighborsFlashed flashes p) | (p,e) <- Map.toList grid]
    flashes0 = Set.fromList [p | (p,e) <- Map.toList grid, e + 1 > 9]
    neighborsFlashed flashes p = length [n | n <- neighbors p, Set.member n flashes]
    flashes = go flashes0
    go flashes
      | flashes' == flashes = flashes
      | otherwise = go flashes'
      where flashes' = Set.fromList [p | (p,e) <- Map.toList grid, e + 1 + neighborsFlashed flashes p > 9]

part1 input = iterate go (input, 0) !! 100
  where
    go (grid, flashes) = let (new_grid, new_flashes) = step grid
                         in (new_grid, flashes + Set.size new_flashes)

part2 input = go input 1
  where
    go grid t
      | Set.size new_flashes == Map.size new_grid = t
      | otherwise = go new_grid (t+1)
      where (new_grid, new_flashes) = step grid
