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

parseInput :: Parser [(V2 Int, V2 Int)]
parseInput = some $ do
  a <- V2 <$> p_int <* char ',' <*> p_int
  string " -> "
  b <- V2 <$> p_int <* char ',' <*> p_int
  spaces
  return (a,b)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/05.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/05_sample.txt")

part1 input = length $ Map.filter (>1) $ Map.fromListWith (+) [(p,1) | p <- points]
  where
    points = foldMap pline input
    pline (V2 x1 y1, V2 x2 y2)
      | x1 == x2 = [V2 x1 y | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2 = [V2 x y1 | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = []


part2 input = length $ Map.filter (>1) $ Map.fromListWith (+) [(p,1) | p <- points]
  where
    points = foldMap pline input
    pline (V2 x1 y1, V2 x2 y2)
      | x1 == x2 = [V2 x1 y | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2 = [V2 x y1 | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = [V2 x y | (x,y) <- zip (ln x1 x2) (ln y1 y2)]
    ln a b | a == b = [a]
           | a < b = [a..b]
           | a > b = [a, a-1 .. b]