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
import           Control.Lens hiding (Empty)
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
-- import qualified Util.Text as T

data Box = Box{lo :: V3 Int, hi :: V3 Int}
  deriving (Show, Eq, Ord)

intersectBox :: Box -> Box -> Maybe Box
intersectBox (Box lo1 hi1) (Box lo2 hi2)
  | and ((<=) <$> lo <*> hi) = Just (Box lo hi)
  | otherwise = Nothing
  where
    lo = max <$> lo1 <*> lo2
    hi = min <$> hi1 <*> hi2

checkbox :: Box -> Bool
checkbox (Box lo hi) = and ((<=) <$> lo <*> hi)

xof,yof,zof :: Box -> (Int,Int)
xof (Box lo hi) = (lo ^. _x, hi ^. _x)
yof (Box lo hi) = (lo ^. _y, hi ^. _y)
zof (Box lo hi) = (lo ^. _z, hi ^. _z)

splitX (a, b) (Box (V3 x1 y1 z1) (V3 x2 y2 z2)) = filter checkbox [Box (V3 x1 y1 z1) (V3 (a-1) y2 z2),
                                                                   Box (V3  a y1 z1) (V3 b y2 z2),
                                                                   Box (V3 (b+1) y1 z1) (V3 x2 y2 z2)]

splitY (a, b) (Box (V3 x1 y1 z1) (V3 x2 y2 z2)) = filter checkbox [Box (V3 x1    y1 z1) (V3 x2 (a-1) z2),
                                                                   Box (V3 x1     a z1) (V3 x2     b z2),
                                                                   Box (V3 x1 (b+1) z1) (V3 x2    y2 z2)]

splitZ (a, b) (Box (V3 x1 y1 z1) (V3 x2 y2 z2)) = filter checkbox [Box (V3 x1 y1    z1) (V3 x2 y2 (a-1)),
                                                                   Box (V3 x1 y1     a) (V3 x2 y2     b),
                                                                   Box (V3 x1 y1 (b+1)) (V3 x2 y2    z2)]

minus :: Box -> Box -> [Box]
minus one two = case intersectBox one two of
  Just two
    | one == two -> []
    | xof one /= xof two -> fold [minus x two | x <- splitX (xof two) one]
    | yof one /= yof two -> fold [minus x two | x <- splitY (yof two) one]
    | zof one /= zof two -> fold [minus x two | x <- splitZ (zof two) one]
  Nothing -> [one]

parseInput :: Parser [(Text, V3 Int, V3 Int)]
parseInput = some $ do
  state <- text "on" <|> text "off"
  spaces
  text "x="
  x1 <- p_int
  text ".."
  x2 <- p_int
  text ",y="
  y1 <- p_int
  text ".."
  y2 <- p_int
  text ",z="
  z1 <- p_int
  text ".."
  z2 <- p_int
  spaces
  return (state, V3 x1 y1 z1, V3 x2 y2 z2)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/22.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/22_sample.txt")

boxSize (Box lo hi) = product (hi + 1 - lo)

part1 steps = foldMap (Sum . boxSize) (foldl apply [] steps)
  where
    apply boxes (_, lo, hi) | Nothing <- intersectBox (Box lo hi) (Box (-50) 50) = boxes
    apply boxes ("on", lo, hi) = fold [minus x (Box lo hi) | x <- boxes] ++ [Box lo hi]
    apply boxes ("off", lo, hi) = fold [minus x (Box lo hi) | x <- boxes]

part2 steps = foldMap (Sum . boxSize) (foldl apply [] steps)
  where
    apply boxes ("on", lo, hi) = fold [minus x (Box lo hi) | x <- boxes] ++ [Box lo hi]
    apply boxes ("off", lo, hi) = fold [minus x (Box lo hi) | x <- boxes]
