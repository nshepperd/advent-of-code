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
import qualified Bag
-- import qualified Util.Text as T

parseInput :: Parser [[V3 Int]]
parseInput = some $ do
  text "--- scanner "
  p_nat
  text " ---"
  spaces
  some $ do
    [x, y, z] <- replicateM 3 (try p_int <* optional (char ',')) <* spaces
    return (V3 x y z)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/19.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/19_sample.txt")

transforms :: [M33 Int]
transforms = [V3 a b c | let [x, y, z] = toList (identity :: M33 Int),
                             x' <- [x, -x],
                             y' <- [y, -y],
                             z' <- [z, -z],
                             [a, b, c] <- permutations [x',y',z'],
                             det33 (V3 a b c) == 1]

oneout :: [a] -> [(a, [a])]
oneout xs = [(m, front ++ back) | (front, m:back) <- zip (inits xs) (tails xs)]

countup :: Ord a => [a] -> [(a, Int)]
countup = Map.toList . Bag.toMap . Bag.fromList

part1 scanners = head $ go (head scanners) [0] (tail scanners)
  where
    go field zeros [] = return (field, zeros)
    go field zeros scanners = do
      (scan, scanners) <- oneout scanners
      tr <- transforms
      let scan' = map (tr !*) scan
      let shifts = [y - x | y <- field, x <- scan']
          (shift, count) = maximumOn snd $ countup shifts
      guard (count >= 12)
      go (Set.toList $ Set.fromList $ field ++ map (shift+) scan') (shift : zeros) scanners

main :: IO ()
main = do
  let (field, zeros) = part1 input
  print (length field)
  print (maximum [sum (abs <$> (a - b)) | a <- zeros, b <- zeros])