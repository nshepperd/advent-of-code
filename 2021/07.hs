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

parseInput :: Parser [Int]
parseInput = some (p_nat <* optional (char ','))

input = unsafePerformIO (parse parseInput <$> T.readFile "input/07.txt")
sample = [16,1,2,0,4,2,7,1,2,14] :: [Int]

part1 input = let m = sort input !! ((length input + 1) `div` 2)
              in sum [abs (x - m) | x <- input]

part2 input = totalFuel target --[totalFuel t | t <- [minimum input .. maximum input]]
  where
    fuel d = sum [1..d]
    totalFuel m = sum [fuel (abs (x - m)) | x <- input]
    Just target = searchFromTo (\m -> totalFuel (m+1) > totalFuel m) (minimum input) (maximum input)
