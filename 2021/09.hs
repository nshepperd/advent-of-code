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


input = unsafePerformIO (parse parseInput <$> T.readFile "input/09.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/09_sample.txt")

neighbors p = [p + d | d <- [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]]

part1 input = sum [h + 1 | (p, h) <- Map.toList input, and [h2 > h | p2 <- neighbors p, Just h2 <- [Map.lookup p2 input]]]

part2 input = product $ map snd $ take 3 $ reverse $ sortOn snd $ Map.toList sizes
  where
    sizes = Map.fromListWith (+) [(pb, 1) | (p, pb) <- Map.toList basins, (input Map.! pb) < 9]
    basins = fmap basin (Map.fromList [(p, p) | p <- Map.keys input])
    basin p
      | h == 9 = p
      | p2 == p = p
      | otherwise = basins Map.! p2
      where
        h = input Map.! p
        ns = [(p2, h2) | p2 <- neighbors p, Just h2 <- [Map.lookup p2 input]]
        (p2, h2) = minimumOn snd ((p,h) : ns)
