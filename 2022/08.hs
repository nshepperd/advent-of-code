{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Data.Foldable
import           Data.Int
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Linear.V2
import           System.IO.Unsafe

import           Util

input :: [[Int]]
(input, sample) = (unsafePerformIO (p <$> T.readFile "input/08.txt"),
                   unsafePerformIO (p <$> T.readFile "sample/08.txt"))
  where
    p = map (map (read . pure)) . lines . T.unpack

part1 input = sum [1 | (x, l) <- zip (concat input) any, x > l]
  where
    f = map (init . scanl max (-1))
    left = f input
    right = (map reverse . f . map reverse) input
    top = (transpose . f . transpose) input
    bot = (reverse . transpose . f . transpose . reverse) input
    any = zipWith min (zipWith min (concat left) (concat right))
                      (zipWith min (concat top) (concat bot))

directions :: [V2 Int]
directions = concat [[V2 0 x, V2 x 0] | x <- [-1, 1]]

part2 input = maximum [product [table Map.! (d,t,p) | d <- directions] | (p, t) <- Map.toList trees]
  where
    w = length (head input)
    h = length input
    grid = Map.keysSet trees
    trees = Map.fromList [(V2 x y, input!!y!!x) | x <- [0..w-1], y <- [0..h-1]]
    table = Map.fromList [((d,t,p), go d t p) | d <- directions, t <- [0..9], p <- Set.toList grid]
    go d t p | edge = 0
             | t <= trees Map.! next = 1
             | True = 1 + (table Map.! (d, t, next))
      where
        next = p + d
        edge = not (Set.member next grid)
