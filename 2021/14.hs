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

import           Bag (Bag)
import qualified Bag as Bag
import           Util

data Rule = Rule (Char,Char) Char
  deriving (Show)

parseInput :: Parser (String, [Rule])
parseInput = do
  template <- some letter
  spaces
  rules <- some $ do
    a <- letter
    b <- letter
    text " -> "
    c <- letter
    spaces
    return (Rule (a,b) c)
  return (template, rules)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/14.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/14_sample.txt")

memoMap :: Ord k => (k -> a) -> [k] -> (k -> a)
memoMap f keys = let table = f <$> Map.fromList [(k,k) | k <- keys]
                 in (table Map.!)

polymerize input steps = let added = fold [counts (a,b) !! steps | (a,b) <- bigrams template]
                             final = Bag.fromList template <> added
                             y = toList (Bag.toMap final)
                         in maximum y - minimum y
  where
    (template, rules) = input
    ruleMap = Map.fromList [((a,b), c) | Rule (a,b) c <- rules]
    alphabet = nub (template ++ fold [[a,b,c] | Rule (a,b) c <- rules])

    counts = memoMap computeCounts [(a,b) | a <- alphabet, b <- alphabet]
    computeCounts (a,b) = [computeCount (a,b) i | i <- [0..]]
    computeCount (a,b) 0 = mempty
    computeCount (a,b) i
      | Just c <- Map.lookup (a,b) ruleMap =
          Bag.singleton c <> (counts (a,c) !! (i-1)) <> (counts (c,b) !! (i-1))
      | otherwise = mempty

part1 input = polymerize input 10

part2 input = polymerize input 40