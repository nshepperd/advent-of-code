{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Lens hiding (Empty)
import           Control.Monad
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Word
import           Debug.Trace
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import           AStar

type Valve = Text

input, sample :: [(Valve, Int, [Valve])]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/16.txt", "sample/16.txt"]
  where
    vname = someText upper
    p = some $ do
      text "Valve "
      name <- vname
      text " has flow rate="
      rate <- p_nat <* char ';' <* spaces
      some (lower <|> char ' ')
      neighbors <- (vname `sepBy` (text ", ")) <* spaces
      return (name, rate, neighbors)

memotab set f = let m = Map.fromList [(x,f x) | x <- toList set]
                in \x -> m Map.! x

part1 input = magical - getSum burnt -- release ("AA", Map.keysSet vsize, 30)
  where
    valves = Map.keysSet vnext
    vsize = Map.fromList [(valve, size) | (valve, size, ns) <- input, size > 0]
    vnext = Map.fromList [(valve, ns) | (valve, size, ns) <- input]
    neighbors v = vnext Map.! v
    dist = memotab valves $ \v1 ->
      let ds = bfs neighbors v1
      in \v2 -> ds Map.! v2

    magical = sum vsize * 30

    burnt = astar r_actions r_step r_goal r_s0 r_output
    r_actions (loc :: Valve, available :: Set Valve, remaining :: Int)
      | remaining == 0 = []
      | Set.null available = wait
      | otherwise = if null nexts then wait else nexts
        where
          burning = sum [vsize Map.! k | k <- toList available]
          wait = [((loc, available, 0), remaining * burning)]
          nexts = do
            turn <- toList available
            let timeSpent = dist loc turn + 1
            guard (timeSpent < remaining)
            let value = (vsize Map.! turn) * (remaining - timeSpent)
            return ((turn, Set.delete turn available, remaining - timeSpent), timeSpent * burning)
    r_step _ n = n
    r_goal (loc :: Valve, available :: Set Valve, remaining :: Int)
      | remaining == 0 = 0
      | otherwise      = 1

    r_s0 = ("AA", Map.keysSet vsize, 30)
    r_output p a s c = Sum c

part2 input = magical - getSum (snd burnt) --magical - (getSum burnt `div` 2) -- release ("AA", Map.keysSet vsize, 30)
  where
    valves = Map.keysSet vnext
    vsize = Map.fromList [(valve, size) | (valve, size, ns) <- input, size > 0]
    vnext = Map.fromList [(valve, ns) | (valve, size, ns) <- input]
    neighbors v = vnext Map.! v
    dist = memotab valves $ \v1 ->
      let ds = bfs neighbors v1
      in \v2 -> ds Map.! v2

    magical = sum vsize * 26

    burnt = astar r_actions r_step r_goal r_s0 r_output
    r_actions (loc1 :: Valve, loc2 :: Valve, available :: Set Valve, rem1 :: Int, rem2 :: Int)
      | max rem1 rem2 == 0 = wait
      | Set.null available = wait
      | rem1 >= rem2 = let wait1 = [((loc1, loc2, available, 0, rem2), (rem1 - rem2) * burning)]
                           next1 = do
                             target <- toList available
                             let timeSpent = dist loc1 target + 1
                             guard (timeSpent < rem1)
                             let rem1' = rem1 - timeSpent
                                 horizon = rem1
                                 horizon' = max rem1' rem2
                                 tburn = vsize Map.! target
                             let cost = (horizon - horizon') * (burning - tburn) + timeSpent * tburn
                             return ((target, loc2, Set.delete target available, rem1 - timeSpent, rem2), cost)
                       in next1 ++ wait1
      | otherwise =    let wait2 = [((loc1, loc2, available, rem1, 0), (rem2 - rem1) * burning)]
                           next2 = do
                             target <- toList available
                             let timeSpent = dist loc2 target + 1
                             guard (timeSpent < rem2)
                             let rem2' = rem2 - timeSpent
                                 horizon = rem2
                                 horizon' = max rem2' rem1
                                 tburn = vsize Map.! target
                             let cost = (horizon - horizon') * (burning - tburn) + timeSpent * tburn
                             return ((loc1, target, Set.delete target available, rem1, rem2 - timeSpent), cost)
                       in next2 ++ wait2
        where
          burning = sum [vsize Map.! k | k <- toList available]
          wait = [((loc1, loc2, Set.empty, 0, 0), max rem1 rem2 * burning)]

    r_step _ n = n
    r_goal (loc1 :: Valve, loc2 :: Valve, available :: Set Valve, rem1 :: Int, rem2 :: Int)
      | max rem1 rem2 == 0 && Set.null available = 0
      | otherwise      = sum [(vsize Map.! v) * min (dist loc1 v + 1) (dist loc2 v + 1) | v <- toList available]

    r_s0 = ("AA", "AA", Map.keysSet vsize, 26, 26)
    r_output p a s c = ([s], Sum c)

-- for constructing the reachability graph
bfs :: (Ord s) => (s -> [s]) -> s -> Map s Int
bfs next s0 = go Map.empty (Seq.singleton (s0,0))
  where
    go visited frontier = case frontier of
      (s,d) :<| frontier
        | Map.member s visited -> go visited frontier
        | otherwise -> go visited' frontier'
        where
          visited' = Map.insert s d visited
          frontier' = frontier <> Seq.fromList [(n, d+1) | n <- next s]
      Empty -> visited

main = do
  print (part2 input)
