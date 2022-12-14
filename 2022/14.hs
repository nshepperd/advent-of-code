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

input, sample :: [[V2 Int]]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/14.txt", "sample/14.txt"]
  where
    p_v2 = V2 <$> p_int <* char ',' <*> p_int
    p_lines = (p_v2 `sepBy1` text " -> ") <* spaces
    p = some $ do
      p_lines <* spaces

part1 input = go Set.empty
  where
    roccSet = foldMap listRocc $ foldMap (\line -> zip line (tail line)) input
    roccIndex = Map.unionsWith (<>) [Map.singleton x (Set.singleton y) | V2 x y <- toList roccSet]
    listRocc (V2 x1 y1, V2 x2 y2)
      | x1 == x2   =   Set.fromList [(V2 x1 y) | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2   =   Set.fromList [(V2 x y1) | x <- [min x1 x2 .. max x1 x2]]

    rocc p = Set.member p roccSet
    roccBelow (V2 x y) = case Set.lookupGE y (Map.findWithDefault Set.empty x roccIndex) of
      Just _ -> True
      Nothing -> False
    go sand = simulate sand (V2 500 0)
    simulate sand p
      | not (roccBelow p) = sand
      | not (blocked (p + V2 0 1)) = simulate sand (p + V2 0 1)
      | not (blocked (p + V2 (-1) 1)) = simulate sand (p + V2 (-1) 1)
      | not (blocked (p + V2 1 1)) = simulate sand (p + V2 1 1)
      | otherwise = go (Set.insert p sand) -- resttt
      where
        blocked q = rocc q || Set.member q sand

part2 input = go Set.empty
  where
    yfloor = maximum [y | V2 x y <- toList roccSet] + 2
    roccSet = foldMap listRocc $ foldMap (\line -> zip line (tail line)) input
    listRocc (V2 x1 y1, V2 x2 y2)
      | x1 == x2   =   Set.fromList [(V2 x1 y) | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2   =   Set.fromList [(V2 x y1) | x <- [min x1 x2 .. max x1 x2]]

    rocc p = Set.member p roccSet
    go sand = simulate sand (V2 500 0)
    simulate sand p
      | blocked p = sand
      | not (blocked (p + V2 0 1)) = simulate sand (p + V2 0 1)
      | not (blocked (p + V2 (-1) 1)) = simulate sand (p + V2 (-1) 1)
      | not (blocked (p + V2 1 1)) = simulate sand (p + V2 1 1)
      | otherwise = go (Set.insert p sand) -- resttt
      where
        blocked q = rocc q || Set.member q sand || floor q
        floor (V2 x y) = y == yfloor
