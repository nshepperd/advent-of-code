{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import           Linear.V3
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import           Util

input, sample :: [V3 Int]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/18.txt", "sample/18.txt"]
  where
    p = some $ do
      [a, b, c] <- (p_int `sepBy` char ',') <* spaces
      return (V3 a b c)

ds :: [V3 Int]
ds = concat [[V3 x 0 0, V3 0 x 0, V3 0 0 x] | x <- [-1,1]]

part1 input = sum [1 | cube <- input, d <- ds, not (Set.member (cube + d) iset)]
  where
    iset = Set.fromList input

part2 input = sum [1 | n <- toList (bfs next vmin), d <- ds, Set.member (n+d) iset]
  where
    next s = [s + d | d <- ds, let n = s + d, and (liftA2 (>=) n vmin), and (liftA2 (<=) n vmax), not (Set.member n iset)]
    vmin = foldr1 (liftA2 min) input - 1
    vmax = foldr1 (liftA2 max) input + 1
    iset = Set.fromList input


bfs :: (Ord s) => (s -> [s]) -> s -> Set s
bfs next s0 = go Set.empty (Seq.singleton (s0,0))
  where
    go visited frontier = case frontier of
      (s,d) :<| frontier
        | Set.member s visited -> go visited frontier
        | otherwise -> go visited' frontier'
        where
          visited' = Set.insert s visited
          frontier' = frontier <> Seq.fromList [(n, d+1) | n <- next s]
      Seq.Empty -> visited
