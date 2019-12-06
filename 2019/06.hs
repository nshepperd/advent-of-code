{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}

import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import           Data.Set (Set)
import qualified Data.Set as Set

import           AStar

parseOrbits :: String -> Map String String
parseOrbits txt = Map.fromList [(b, a) | [a, b] <- map (splitOn ")") (lines txt)]

countOrbits :: Map String String -> Map String Int
countOrbits orb = ct
  where
    ct = fmap (\v -> Map.findWithDefault 0 v ct + 1) orb

solve1 :: String -> Int
solve1 txt = sum $ countOrbits $ parseOrbits txt

solve2 :: String -> Int
solve2 txt = astar (\p -> step Map.! p) (\_ _ -> 1) (\p -> if p == (orb Map.! "SAN") then 0 else 1) (orb Map.! "YOU")
  where
    orb = parseOrbits txt
    rev = Map.fromListWith (++) [(b, [a]) | (a, b) <- Map.toList orb]
    step = Map.unionWith (++) (pure <$> orb) rev
