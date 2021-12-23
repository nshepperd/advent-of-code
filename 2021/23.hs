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
import           Control.Lens ((^.))
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

type Grid a = Map (V2 Int) a

parseInput :: Parser (Grid Char)
parseInput = do
  lines <- some (some (noneOf ['\n']) <* optional (char '\n'))
  return $ Map.fromList [(V2 x y, c)
                        | (y, line) <- zip [0..] lines,
                          (x, c) <- zip [0..] line]


input = unsafePerformIO (parse parseInput <$> T.readFile "input/23.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/23_sample.txt")

neighbors p = [p + d | dx <- [-1, 0, 1], dy <- [-1, 0, 1], let d = V2 dx dy, sum (abs <$> d) == 1]

reachability :: (Ord s) => (s -> [s]) -> s -> Set s
reachability next s0 = go Set.empty [s0]
  where
    go visited [] = visited
    go visited (s:ss)
      | Set.member s visited = go visited ss
      | otherwise = go visited' ss'
      where
        visited' = Set.insert s visited
        ss' = next s ++ ss


part1 input = astar actions step goal start output
  where
    rooms = Set.fromList [p | (p, c) <- Map.toList input, elem c ['A'..'D']]
    doorways = Set.fromList [p - V2 0 1 | p <- toList rooms] `Set.difference` rooms
    roomsByType = Map.fromList [(c, Set.filter (\p -> p ^. _x == x) rooms) | (c, x) <- zip "ABCD" (sort (nub [x | V2 x y <- toList rooms]))]
    floor = Set.fromList [p | (p, c) <- Map.toList input, elem c ['.']] `Set.difference` doorways
    walkable = (rooms <> doorways <> floor)
    reachableByPoint = Map.fromList $ do
      p <- toList walkable
      o <- toList walkable
      guard (o /= p)
      let s = reachability (\p -> [n | n <- neighbors p, Set.member n walkable, n /= o]) p
      return ((p,o), s)
    reachableWithObstacles p os = foldr Set.intersection walkable [reach | o <- os, Just reach <- [Map.lookup (p, o) reachableByPoint]]

    distanceMap = Map.fromList $ do
      p <- toList walkable
      d <- toList walkable
      guard (d /= p)
      let distance = getSum $ astar (\p -> [n | n <- neighbors p, Set.member n walkable]) (\p n -> (n,1)) (\n -> if n == d then 0 else 1) p (\p a n c -> Sum c)
      return ((p,d), distance)

    start = Map.fromListWith (<>) [(c, Set.singleton p) | (p, c) <- Map.toList input, elem c ['A'..'D']]
    actions pods = do
      (c, ps) <- Map.toList pods
      p <- toList ps
      d <- case Set.member p floor of
             True -> let nonc = fold [pods Map.! c2 | c2 <- ['A'..'D'], c2 /= c]
                     in if Set.null (Set.intersection nonc (roomsByType Map.! c)) then  toList (roomsByType Map.! c) else []
             False -> toList floor
      guard (d `Set.member` reachableWithObstacles p (toList (fold pods)))
      return (c,p,d)
    step pods (c,p,d) = let ps = pods Map.! c
                            ps' = Set.insert d (Set.delete p ps)
                            pods' = Map.insert c ps' pods
                            cost = distanceFor c p d
                        in (pods',cost)

    distanceFor c p d = energyCost c * distance p d
    distance p d | p == d = 0
                 | otherwise = distanceMap Map.! (p,d)
    energyCost 'A' = 1
    energyCost 'B' = 10
    energyCost 'C' = 100
    energyCost 'D' = 1000

    emDistance ps ds
      | Set.null ps && Set.null ds = 0
      | otherwise = let (cost, p, d) = minimum [(distanceFor 'A' p d, p, d) | p <- toList ps, d <- toList ds]
                    in cost + emDistance (Set.delete p ps) (Set.delete d ds)

    goal1 pods = sum [energyCost c * emDistance (pods Map.! c) (roomsByType Map.! c) |  c <- ['A'..'D']]

    goal pods
      | not (null moveouts) = 2 * sum moveouts + goal1 pods
      | otherwise = goal1 pods
      where
        moveouts = do
          c <- ['A'..'D']
          p <- toList (pods Map.! c)
          -- We are in our room
          guard $ Set.member p (roomsByType Map.! c)
          -- But there is someone who needs to get past
          guard $ or [True | c2 <- ['A'..'D'], c2 /= c, p2 <- toList (pods Map.! c2), (p2^._x == p^._x), (p2^._y > p^._y)]
          -- Move to the floor and then back
          return $ minimum [distanceFor c p f | f <- toList floor]

    output pods _ pods' cost = Sum cost

main :: IO ()
main = do
  print (part1 input)