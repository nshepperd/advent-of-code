{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module Search where

import           Data.Bool
import           Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

insertsH :: (Ord p) => [(p, v)] -> MinPrioHeap p v -> MinPrioHeap p v
insertsH items queue = foldr H.insert queue items

astarOn :: (Ord k)
        => (s -> k)
        -> (s -> [(s,Int)]) -- actions
        -> (s -> Int)       -- goal
        -> s                -- start
        -> [s]
astarOn key actions goal start = go Set.empty (H.singleton (goal start, (start,0)))
  where
    go visited frontier = case H.view frontier of
      Just ((_, (s, cost)), frontier)
        | Set.member k visited -> go visited frontier
        | goal s == 0    -> s : go visited' frontier'
        | otherwise      -> go visited' frontier'
          where
            k = key s
            visited' = Set.insert k visited
            frontier' = insertsH [(cost' + goal s', (s', cost')) |
                                   (s', c') <- actions s, let cost' = cost + c'] frontier
      Nothing -> []

bfsOn :: (Ord k)
      => (s -> k)
      -> (s -> [s])    -- actions
      -> (s -> Bool)   -- goal
      -> s             -- start
      -> [s]
bfsOn key actions goal start = astarOn key (\s -> [(s', 1) | s' <- actions s]) (bool 1 0 . goal) start
