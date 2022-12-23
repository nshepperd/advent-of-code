{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Linear
import           System.IO
import           System.IO.Unsafe

import           Util

type Pos = V2 Int

input, sample :: [String]
[input, sample] = fmap (lines . T.unpack . unsafePerformIO . T.readFile) ["input/23.txt", "sample/23.txt"]

dirs :: [V2 Int]
dirs = [
  V2 0 (-1),
  V2 0 1,
  V2 (-1) 0,
  V2 1 0
  ]

cross2 (V2 x y) = V2 y x

dcheck :: Pos -> V2 Int -> Set Pos -> Bool
dcheck p d elves = and [Set.notMember (p + n) elves | n <- [d - cross2 d, d, d + cross2 d]]

neighbors :: Pos -> [Pos]
neighbors p = [p + V2 dx 0 + V2 0 dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1], V2 dx 0 + V2 0 dy /= 0]

acheck :: Pos -> Set Pos -> Bool
acheck p elves = or [Set.member n elves | n <- neighbors p]

draw (elves, i) = do print i
                     putStrLn $ unlines [[if V2 x y `Set.member` elves then '#' else '.' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    V2 xmin ymin = foldr1 (liftA2 min) elves
    V2 xmax ymax = foldr1 (liftA2 max) elves

solve input = iterate step (start, 0)
  where
    start = Set.fromList [(V2 x y) | (y, line) <- zip [0..] input, (x, '#') <- zip [0..] line]
    step (elves, i) = (elves', i+1)
      where
        moves = Map.fromSet trymove elves
        trymove p = head $ [p + d | d <- take 4 $ drop (i `mod` 4) $ cycle $ dirs, dcheck p d elves, acheck p elves] ++ [p]
        allowed = Set.fromList [n | (n, 1) <- Map.toList $ Map.fromListWith (+) [(n,1::Int) | n <- toList moves]]
        elves' = Set.fromList [if Set.member n allowed then n else p | (p, n) <- Map.toList moves]


part1 input = finish $ fst $ solve input !! 10
  where
    finish elves = (ymax-ymin+1)*(xmax-xmin+1) - Set.size elves
      where
        V2 xmin ymin = foldr1 (liftA2 min) elves
        V2 xmax ymax = foldr1 (liftA2 max) elves

part2 input = head $ [i | ((e1, _), (e2, i)) <- (zip`ap`tail) (solve input), e1 == e2]
