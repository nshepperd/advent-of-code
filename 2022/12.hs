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
import           Control.Lens
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
import           Data.List.Split (chunksOf)
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
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import           AStar

input, sample :: [[Char]]
[input, sample] = fmap (lines . T.unpack . unsafePerformIO . T.readFile) ["input/12.txt", "sample/12.txt"]

neighbors x = concat [[x + V2 d 0, x + V2 0 d] | d <- [-1, 1]]

elevation 'S' = 0
elevation 'E' = 25
elevation c | 'a' <= c && c <= 'z'  = ord c - ord 'a'

part1 input = astar next step goal start (\p a n c -> Sum c)
  where
    start = head [V2 x y | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, c == 'S']
    end = head [V2 x y | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, c == 'E']
    grid = Map.fromList [(V2 x y, elevation c) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line]
    next p = [n | n <- neighbors p, Map.member n grid, sum (abs <$> (n - p)) == 1, (grid Map.! n - grid Map.! p) <= 1]
    step p n = (n, 1)
    goal p = sum (abs <$> (p - end))

part2 input = astar next step goal end (\p a n c -> Sum c)
  where
    end = head [V2 x y | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, c == 'E']
    grid = Map.fromList [(V2 x y, elevation c) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line]
    next p = [n | n <- neighbors p, Map.member n grid, sum (abs <$> (n - p)) == 1, (grid Map.! n - grid Map.! p) >= (-1)]
    step p n = (n, 1)
    goal p = grid Map.! p

plot1 input = putStr $ unlines $ [concat [if Set.member (V2 x y) visited then "\x1b[41m"++[c]++"\x1b[m" else [c] | (x, c) <- zip [0..] line] | (y, line) <- zip [0..] input]
  where
    visited = Set.fromList $ astar next step goal start (\p a n c -> [n])
    start = head [V2 x y | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, c == 'S']
    end = head [V2 x y | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, c == 'E']
    grid = Map.fromList [(V2 x y, elevation c) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line]
    next p = [n | n <- neighbors p, Map.member n grid, sum (abs <$> (n - p)) == 1, (grid Map.! n - grid Map.! p) <= 1]
    step p n = (n, 1)
    -- goal p = sum (abs <$> (p - end))
    goal p | p == end = 0
           | otherwise = 1 `max` sum (abs <$> (p - end)) `max` 25 - grid Map.! p
    -- step p n = (n, 1000)
    -- goal p | p == end = 0
    --        | otherwise = 1000 - sum (abs <$> (p - end))
