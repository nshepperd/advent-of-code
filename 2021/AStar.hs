{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module AStar where

import qualified Data.OrdPSQ as PSQ
import           Data.OrdPSQ (OrdPSQ)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

insertsPSQ :: (Ord k, Ord p) => [(k, p, v)] -> OrdPSQ k p v -> OrdPSQ k p v
insertsPSQ items queue = foldr (\(k,p,v) queue -> PSQ.insert k p v queue) queue items

astar :: (Ord s, Monoid m)
      => (s -> [a]) -> (s -> a -> (s, Int))
      -> (s -> Int) -> s -> (s -> a -> s -> Int -> m) -> m
astar actions step goal s0 output = go Set.empty (PSQ.singleton (s0,0) 0 (0, mempty))
  where
    go visited frontier = case PSQ.minView frontier of
      Just ((s,_), _, (cost, path), frontier)
        | Set.member s visited -> go visited frontier
        | h == 0 -> path
        | otherwise -> go visited' frontier'
          where
            h = goal s
            visited' = Set.insert s visited
            frontier' = insertsPSQ [((s',cost + c'), cost + c' + goal s', (cost + c', path <> output s a s' c'))  |
                                     a <- actions s, let (s', c') = step s a] frontier
      Nothing -> error "No path found!"
