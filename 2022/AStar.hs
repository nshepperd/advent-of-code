{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module AStar where

import qualified Data.OrdPSQ as PSQ
import           Data.OrdPSQ (OrdPSQ)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

-- inserts the (key, priority, value) tuple into the queue, keeping
-- whichever has lowest priority if the key is already present.
insertKeepMin :: (Ord k, Ord p) => (k, p, v) -> OrdPSQ k p v -> OrdPSQ k p v
insertKeepMin (k,p,v) queue = case PSQ.lookup k queue of
  Just (p2, v2) | p2 <= p -> queue
  otherwise               -> PSQ.insert k p v queue

insertsPSQ :: (Ord k, Ord p) => [(k, p, v)] -> OrdPSQ k p v -> OrdPSQ k p v
insertsPSQ items queue = foldr insertKeepMin queue items

astar :: (Ord s, Monoid m)
      => (s -> [a]) -> (s -> a -> (s, Int))
      -> (s -> Int) -> s -> (s -> a -> s -> Int -> m) -> m
astar actions step goal s0 output = go Set.empty (PSQ.singleton s0 0 (0, mempty))
  where
    go visited frontier = case PSQ.minView frontier of
      Just (s, _, (cost, path), frontier)
        | Set.member s visited -> go visited frontier
        | h == 0 -> path
        | otherwise -> go visited' frontier'
          where
            h = goal s
            visited' = Set.insert s visited
            frontier' = insertsPSQ [(s', cost + c' + goal s', (cost + c', path <> output s a s' c'))  |
                                     a <- actions s, let (s', c') = step s a] frontier
      Nothing -> error "No path found!"
