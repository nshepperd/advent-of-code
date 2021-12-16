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

data Fold = FoldLeft Int | FoldUp Int
  deriving (Show, Eq, Ord)

parseInput :: Parser (Set (V2 Int), [Fold])
parseInput = do
  pts <- some $ do
    x <- p_int <* char ','
    y <- p_int <* char '\n'
    return (V2 x y)
  spaces
  folds <- some $ do
    text "fold along "
    axis <- oneOf "xy"
    char '='
    val <- p_int
    spaces
    return $ case axis of
      'x' -> FoldLeft val
      'y' -> FoldUp val
  return (Set.fromList pts, folds)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/13.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/13_sample.txt")

step :: Set (V2 Int) -> Fold -> Set (V2 Int)
step pts (FoldLeft x) = Set.fromList [if px < x then V2 px py else V2 (x - (px - x)) py | V2 px py <- Set.toList pts]
step pts (FoldUp y) = Set.fromList [if py < y then V2 px py else V2 px (y - (py - y)) | V2 px py <- Set.toList pts]

part1 (grid, folds) = length $ step grid (head folds)

part2 (grid, folds) = draw (foldl step grid folds)
  where
    draw grid = putStrLn $ unlines $ [[if Set.member (V2 x y) grid then '#' else '.' | x <- [0..max_x]] | y <- [0..max_y]]
      where
        max_x = maximum [x | V2 x y <- Set.toList grid]
        max_y = maximum [y | V2 x y <- Set.toList grid]
