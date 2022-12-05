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

type Range = (Int,Int)

input :: [(Range,Range)]
(input,sample) = (unsafePerformIO (parse p <$> T.readFile "input/04.txt"),
                  unsafePerformIO (parse p <$> T.readFile "sample/04.txt"))
  where
    p_range = do
      l <- p_int
      char '-'
      r <- p_int
      return (l,r)
    p = some $ do
      a <- p_range
      char ','
      b <- p_range
      spaces
      return (a,b)

inter :: Range -> Range -> Bool
inter (a,b) (c,d) = max a c <= min b d

sub :: Range -> Range -> Bool
sub (a,b) (c,d) = a >= c && b <= d

part1 input = sum (map part input)
  where
    part (a,b)
      | a `sub` b = 1
      | b `sub` a = 1
      | otherwise = 0

part2 input = sum (map part input)
  where
    part (a,b)
      | inter a b = 1
      | otherwise = 0
