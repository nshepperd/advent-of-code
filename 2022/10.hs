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
import           Data.List.Split (chunksOf)
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


data Op = Noop | Addx Int
  deriving (Show)

input :: [Op]
(input, sample) = over each (parse p . unsafePerformIO . T.readFile) ("input/10.txt",
                                                            "sample/10.txt")
  where
    p = some $ do
      ((Noop <$ text "noop") <|> (Addx <$> (text "addx " *> p_int))) <* spaces

part1 input = sum $ map (uncurry (*)) $ map head $ chunksOf 40 $ drop 19 $ zip [1..] $ foldr go finish input 1
  where
    go Noop k x = x : k x
    go (Addx n) k x = [x, x] ++ k (x + n)
    finish x = []

part2 input = putStrLn $ unlines $ map concat $ chunksOf 40 $ lit
  where
    lit = [if abs (x - dx) <= 1 then "██" else "░░"  | (V2 x y, dx) <- zip positions xs]
    xs = foldr go finish input 1
    positions = [V2 x y | y <- [0..5], x <- [0..39]]
    go Noop k x = x : k x
    go (Addx n) k x = [x, x] ++ k (x + n)
    finish x = []
