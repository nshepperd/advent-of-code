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

data Target = Target{xmin::Int, xmax::Int, ymin::Int, ymax::Int}
  deriving (Show)

parseInput :: Parser Target
parseInput = do
  text "target area: x="
  xmin <- p_int
  text ".."
  xmax <- p_int
  text ", y="
  ymin <- p_int
  text ".."
  ymax <- p_int
  return Target{..}

input = parse parseInput "target area: x=137..171, y=-98..-73"
sample = parse parseInput "target area: x=20..30, y=-10..-5"

step (t, p, v@(V2 vx vy)) = (t+1, p + v, v - V2 (signum vx) 1)

stepN (t, p, v@(V2 vx vy)) dt = (t + dt,
                                 p + V2 delta_x delta_y,
                                 v - V2 (min dt vx) dt)
  where
    tri dt = div (dt*(dt-1)) 2
    delta_y = vy * dt - tri dt
    delta_x | dt <= vx = vx * dt - tri dt
            | dt > vx = vx * vx - tri vx

top yvel = yvel*yvel - tri yvel
  where
    tri dt = div (dt*(dt-1)) 2

part1 Target{..} = head [(top yvel) | yvel <- reverse [0..max_y_vel], Just xvel <- solve yvel]
  where
    max_y_vel = abs ymin - 1
    max_x_vel = xmax
    solve yvel = do
      t <- map (\(t, p, v) -> t) $ filter (\(t, p, v) -> p^._y <= ymax) $ takeWhile (\(t, p, v) -> p^._y >= ymin || v^._y >= 0) $ iterate step (0, 0, (V2 0 yvel))
      xvel <- [0..max_x_vel]
      let (_, p, _) = stepN (0, 0, V2 xvel yvel) t
      guard (xmin <= p^._x && p^._x <= xmax)
      return (Just xvel)

part2 Target{..} = length $ nub [(xvel, yvel) | yvel <- reverse [ymin..max_y_vel], Just xvel <- solve yvel]
  where
    max_y_vel = abs ymin - 1
    max_x_vel = xmax
    solve yvel = do
      t <- map (\(t, p, v) -> t) $ filter (\(t, p, v) -> p^._y <= ymax) $ takeWhile (\(t, p, v) -> p^._y >= ymin || v^._y >= 0) $ iterate step (0, 0, (V2 0 yvel))
      xvel <- [0..max_x_vel]
      let (_, p, _) = stepN (0, 0, V2 xvel yvel) t
      guard (xmin <= p^._x && p^._x <= xmax)
      return (Just xvel)
