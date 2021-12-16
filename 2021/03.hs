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

parseInput :: Parser [String]
parseInput = some (some digit <* spaces)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/03.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/03_sample.txt")

binary xs = foldl (\n digit -> case digit of
                      '0' -> n * 2
                      '1' -> n * 2 + 1) 0 xs

part1 input = binary [if length (filter (=='1') x) >= length x `div` 2 then '1' else '0'
                     | x <- transpose input]
              * binary [if length (filter (=='1') x) >= length x `div` 2 then '0' else '1'
                     | x <- transpose input]

part2 input = (go oxy input 0 * go co2 input 0)
  where
    go criterion [n] _ = binary n
    go criterion xs i = let b = criterion (map (!!i) xs)
                            xs' = filter (\num -> (num !! i) == b) xs
                        in go criterion xs' (i+1)
    ones bs = length (filter (=='1') bs)
    zeros bs = length (filter (=='0') bs)
    oxy bs = if ones bs >= zeros bs then '1' else '0'
    co2 bs = if ones bs >= zeros bs then '0' else '1'