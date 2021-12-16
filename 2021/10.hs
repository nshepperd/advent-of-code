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

parseInput :: Parser [String]
parseInput = some (some (oneOf "([{<>}])") <* spaces)


input = unsafePerformIO (parse parseInput <$> T.readFile "input/10.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/10_sample.txt")

rev '(' = ')'
rev '{' = '}'
rev '[' = ']'
rev '<' = '>'
rev ')' = '('
rev '}' = '{'
rev ']' = '['
rev '>' = '<'


elems c s = elem c (s :: String)

data Result = Valid | Invalid Char | Incomplete String
  deriving (Show)

pline line = go [] line
  where
    go [] [] = Valid
    go (o:os) (c:cs)
      | elems c "([{<" = go (c:o:os) cs
      | elems c ")]}>", c == rev o = go os cs
      | elems c ")]}>", c /= rev o = Invalid c
    go (o:os) [] = Incomplete (o:os)
    go [] (c:cs)
      | elems c "([{<" = go [c] cs
      | elems c ")]}>" = Invalid c

part1 input = sum [score c | Invalid c <- map pline input]
  where
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137

part2 input = rs !! ((length rs) `div` 2)
  where
    rs = sort [score (map rev cs) | Incomplete cs <- map pline input]
    score cs = foldl (\acc c -> 5 * acc + scorec c) 0 cs
    scorec ')' = 1
    scorec ']' = 2
    scorec '}' = 3
    scorec '>' = 4