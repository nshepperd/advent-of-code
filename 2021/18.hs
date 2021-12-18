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

data Snail = Lit Int | Pair Snail Snail
  deriving (Show, Eq, Ord)

parseInput :: Parser [Snail]
parseInput = some (p_snail <* spaces)
  where
    p_snail = p_lit <|> p_pair
    p_lit = Lit <$> p_int
    p_pair = do
      char '['
      a <- p_snail
      char ','
      b <- p_snail
      char ']'
      return (Pair a b)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/18.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/18_sample.txt")

reduce :: Snail -> Snail
reduce snail = go snail
  where
    go snail = case tryExplode snail <|> trySplit snail of
      Just snail -> go snail
      Nothing -> snail
    tryExplode snail = case goExplode snail 0 of
                         Just (_, snail, _) -> Just snail
                         Nothing -> Nothing
    goExplode (Lit v) d = Nothing
    goExplode (Pair (Lit l) (Lit r)) d
      | d >= 4 = Just (Just l, Lit 0, Just r)
      | otherwise = Nothing
    goExplode (Pair l r) d
      | Just (a, l', b) <- goExplode l (d + 1) = Just (a, Pair l' (addLeft b r), Nothing)
      | Just (a, r', b) <- goExplode r (d + 1) = Just (Nothing, Pair (addRight a l) r', b)
      | otherwise = Nothing

    addLeft Nothing v = v
    addLeft (Just a) (Lit v) = Lit (a + v)
    addLeft (Just a) (Pair l r) = Pair (addLeft (Just a) l) r
    addRight Nothing v = v
    addRight (Just a) (Lit v) = Lit (a + v)
    addRight (Just a) (Pair l r) = Pair l (addRight (Just a) r)

    trySplit (Lit v)
      | v >= 10 = Just (Pair (Lit (div v 2)) (Lit (v - div v 2)))
      | otherwise = Nothing
    trySplit (Pair l r)
      | Just l <- trySplit l = Just (Pair l r)
      | Just r <- trySplit r = Just (Pair l r)
      | otherwise = Nothing


magnitude :: Snail -> Int
magnitude (Lit v) = v
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

addFish a b = reduce (Pair a b)

part1 input = magnitude (foldl1 addFish input)
part2 input = maximum [magnitude (addFish a b) | a <- input, b <- input, a /= b]