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
import           Control.Lens hiding (Empty)
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

input = (10,4)
sample = (4,8)

memoMap :: Ord k => (k -> a) -> [k] -> (k -> a)
memoMap f keys = let table = f <$> Map.fromList [(k,k) | k <- keys]
                 in (table Map.!)

part1 start = play start (0,0) 0 (cycle [1..100])
  where
    play (p1,p2) (s1,s2) r die
      | s1 + new_p1 >= 1000 = s2 * (r + 3)
      | otherwise = play (p2, new_p1) (s2, s1 + new_p1) (r+3) (drop 3 die)
      where
        new_p1 = 1 + mod (p1 + sum (take 3 die) - 1) 10

part2 (p1,p2) = wins (p1, p2, 0, 0)
  where
    wins = memoMap go [(p1, p2, s1, s2) | p1 <- [1..10], p2 <- [1..10], s1 <- [0..20], s2 <- [0..20]]
    go (p1, p2, s1, s2) = sumWins $ do
      d1 <- [1..3]
      d2 <- [1..3]
      d3 <- [1..3]
      let d = d1 + d2 + d3
          p1' = 1 + mod (p1 + d - 1) 10
          next | s1 + p1' >= 21 = (1, 0)
               | otherwise      = case wins (p2, p1', s2, s1 + p1') of
                                    (a, b) -> (b, a)
      return next
    sumWins xs = let (as, bs) = unzip xs in (sum as, sum bs)