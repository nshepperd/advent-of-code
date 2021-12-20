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

type Grid a = Map (V2 Int) a

parseInput :: Parser (Vector Char, Grid Char)
parseInput = do
  let bit = oneOf ['#','.']
  pat <- replicateM 512 bit
  spaces
  lines <- some (some bit <* spaces)
  return $ (V.fromList pat,
            Map.fromList [(V2 x y, c)
                         | (y, line) <- zip [0..] lines,
                           (x, c) <- zip [0..] line])

input = unsafePerformIO (parse parseInput <$> T.readFile "input/20.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/20_sample.txt")

neighborhood :: V2 Int -> [V2 Int]
neighborhood p = [p + V2 x y | y <- [-1..1], x <- [-1..1]]

binary :: String -> Int
binary xs = foldl (\n digit -> case digit of
                      '.' -> n * 2
                      '#' -> n * 2 + 1) 0 xs

step :: Vector Char -> (Bool, Set (V2 Int)) -> (Bool, Set (V2 Int))
step pat (False, grid) = (True, Set.fromList [p | p <- to_check, step_point p == '.'])
  where
    to_check = Set.toList $ Set.fromList [p2 | p <- toList grid, p2 <- neighborhood p]
    step_point p = let total = binary [if Set.member n grid then '#' else '.' | n <- neighborhood p]
                   in pat V.! total
step pat (True, grid) = (False, Set.fromList [p | p <- to_check, step_point p == '#'])
  where
    to_check = Set.toList $ Set.fromList [p2 | p <- toList grid, p2 <- neighborhood p]
    step_point p = let total = binary [if Set.member n grid then '.' else '#' | n <- neighborhood p]
                   in pat V.! total

part1 (pat, text_grid) = Set.size (snd $ iterate (step pat) grid !! 2)
  where
    grid = (False, Map.keysSet (Map.filter (=='#') text_grid) :: Set (V2 Int))

part2 (pat, text_grid) = Set.size (snd $ iterate (step pat) grid !! 50)
  where
    grid = (False, Map.keysSet (Map.filter (=='#') text_grid) :: Set (V2 Int))
