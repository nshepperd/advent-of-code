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

parseInput :: Parser [([String], [String])]
parseInput = some $ do
  signals <- some (some letter <* optional (char ' '))
  char '|' >> spaces
  outputs <- some (some letter <* optional (char ' '))
  spaces
  return (signals, outputs)


input = unsafePerformIO (parse parseInput <$> T.readFile "input/08.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/08_sample.txt")

mapOf as f = sequence $ Map.fromList [(a, f) | a <- toList as]
options as = do opts <- mapOf as (E.exists :: Ersatz E.Bit)
                E.assert (exactlyOne (toList opts))
                return opts

crossw wires = do
  let table = toList <$> toList wires
  traverse_ (E.assert . exactlyOne) table
  traverse_ (E.assert . exactlyOne) (transpose table)

unOptions :: Map a Bool -> a
unOptions mp = head [m | (m, True) <- Map.toList mp]

mkPat :: String -> Map Char E.Bit
mkPat xs = E.encode $ Map.fromList [(a, elem a xs) | a <- ['a'..'g']]

solve (signals, outputs) = do
  wires <- mapOf ['a'..'g'] $ options ['a'..'g']
  let wpairs = [(x, y, b) | (x, sub) <- Map.toList wires, (y, b) <- Map.toList sub]
  crossw wires
  signal_digits <- traverse (\_ -> options [0..9]) signals
  output_digits <- traverse (\_ -> options [0..9]) outputs
  let connect dopts seen = do
        decoded <- mapOf ['a'..'g'] (E.exists :: Ersatz E.Bit)
        E.assert $ E.and [is_xy E.==> (if elem x seen then decoded Map.! y else E.not (decoded Map.! y)) | (x, y, is_xy) <- wpairs]
        E.assert $ E.and [is_d E.==> (decoded E.=== pat d) | (d, is_d) <- Map.toList dopts]
      pat 0 = mkPat "abcefg"
      pat 1 = mkPat "cf"
      pat 2 = mkPat "acdeg"
      pat 3 = mkPat "acdfg"
      pat 4 = mkPat "bcdf"
      pat 5 = mkPat "abdfg"
      pat 6 = mkPat "abdefg"
      pat 7 = mkPat "acf"
      pat 8 = mkPat "abcdefg"
      pat 9 = mkPat "abcdfg"
  sequence [connect dopts seen | (dopts, seen) <- zip signal_digits signals]
  sequence [connect dopts seen | (dopts, seen) <- zip output_digits outputs]
  return (output_digits)

part1 input = length [x | x <- fold digits, elem x [1,4,7,8]]
  where
    digits = [map unOptions $ head $ solve1 (solve entry) | entry <- input]

part2 input = sum outputs
  where
    digits = [map unOptions $ head $ solve1 (solve entry) | entry <- input]
    decimal ds = foldl (\acc d -> acc * 10 + d) 0 ds
    outputs = [decimal o | o <- digits]
