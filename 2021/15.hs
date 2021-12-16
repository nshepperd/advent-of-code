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

type Grid a = Map (V2 Int) a

parseInput :: Parser (Grid Int)
parseInput = do
  lines <- some (some digit <* spaces)
  return $ Map.fromList [(V2 x y, read [c] :: Int)
                        |
                         (y, line) <- zip [0..] lines,
                         (x, c) <- zip [0..] line]


input = unsafePerformIO (parse parseInput <$> T.readFile "input/15.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/15_sample.txt")

neighbors p = [p + d | dx <- [-1, 0, 1], dy <- [-1, 0, 1], let d = V2 dx dy, sum (abs <$> d) == 1]

part1 grid = astar actions step goal s0 out
  where
    targetPos = maximum (Map.keys grid)
    actions p = [n | n <- neighbors p, Map.member n grid]
    step p a = (a, grid Map.! a)
    goal p = sum (abs <$> (p - targetPos)) -- L1 distance
    s0 = V2 0 0
    out p a n c = Sum c

part2 grid = part1 big_grid
  where
    gridSize = V2 1 1 + maximum (Map.keys grid)
    big_grid = Map.fromList [(p + (V2 kx ky) * gridSize,
                              let c' = c + kx + ky in mod (c' - 1) 9 + 1)
                            | kx <- [0..4], ky <- [0..4], (p, c) <- Map.toList grid]
