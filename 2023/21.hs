{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Linear.V2
import           Linear.Vector
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators (eof)
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

data Dir = U | D | L | R
  deriving (Show, Eq, Ord, Read)

dv :: Dir -> V2 Int
dv U = -V2 0 1
dv D = V2 0 1
dv L = -V2 1 0
dv R = V2 1 0

input, sample :: [String]
[input, sample] = fmap (lines . unsafePerformIO . readFile) ["input/21.txt", "sample/21.txt"]

part1 input = Set.size $ Set.fromList [pos | (pos,s) <- bfsOn key actions (const True) (start, 0),
                                       s <= 64,
                                       s `mod`2 == 0
                                       ]
  where
    start = head [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, c == 'S']
    floor = Set.fromList [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, elem c ['S','.']]
    key (pos, steps) = (pos, mod steps 2)
    actions (pos, steps) = [(pos + dv d, steps + 1) | d <- [U,D,L,R], Set.member (pos + dv d) floor]

-- triangular numbers. sum [1..n]
tri :: Int -> Int
tri n = (n * (n+1)) `div` 2

-- sum [a | a <- [1..n], a `mod` 2 == phase]
halftri :: Int -> Int -> Int
halftri 0 n =
  --[a | a <- [0..n], a `mod` 2 == 0]
  --[a | a <- [0,2..n]]
  --[2a | a <- [0,1..n/2]]
  -- 2 tri(n/2)
  2 * tri (n`div`2)
halftri 1 n = tri n - halftri 0 n

assert :: Bool -> ()
assert True = ()
assert False = error "assertion"

manhatten :: V2 Int -> Int
manhatten d = sum (abs <$> d)

part2 input = (sum fast, ref)
  where
    target = 26501365
    height = length input
    width = length (head input)
    size = width
    !_ = assert (width == height)
    !_ = assert (size `mod` 2 == 1)

    fast = map sum (
      [[1 | (pos,s) <- Map.toList (pre Map.! start), s <= target, mod s 2 == mod target 2]] ++
      [[calculate pos s anchor | (pos,s) <- Map.toList dists]
      | (anchor, dists) <- Map.toList pre, anchor /= start])
    calculate pos s anchor
      -- Basic idea:
      -- The grid can be considered as a checkered board, like the reachable squares of a bishop.
      -- You can reach a square in exactly `target` steps if you can reach it in <=target steps and the parity of the manhatten distance is the same as `target`.
      -- s1 is the steps taken to get from `start` to an anchor (corner, or middle of edge) of one of the 8 neighboring grids.
      -- s3 is the steps taken to get from there to `pos`
      -- so we can reach `pos` in exactly `target` steps in all of the grid copies such that s1 + s2 + s3 <= target, where s2 is the time taken to walk to the same anchor of that grid.
      -- (this relies on the shortest path always passing through these 'anchors', which is true of the input but not the sample grid.)
      -- the size of the grids is odd, so the parity of s2 is the parity of the number of grids traveled
      | sum (fmap (abs.signum) (anchor-start)) == 2 = -- corner
        -- sum [1+a | a <- [0..s2w], a `mod` 2 == phase]
        halftri (1-phase) (s2w+1)
      | sum (fmap (abs.signum) (anchor-start)) == 1 = -- edge
        -- sum [1 | a <- [0..s2w], a `mod` 2 == phase]
        1 + div (s2w-phase) 2
      where
        anchor1 = anchor - pure size * signum (anchor-start)
        s1 = manhatten (start - anchor1)
        s3 = s
        s2max = target - s1 - s3
        phase = s2max `mod` 2
        s2w = s2max `div` size
    pre = let V2 sx sy = start in Map.fromList [(V2 x y, Map.fromList $ bfsOn key actions (const True) (V2 x y, 0)) | x <- [0, sx, width-1], y <- [0, sy, height-1]]
    wrap pos = (`mod`size) <$> pos
    floor = Set.fromList [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, elem c ['S','.']]

    -- Search within a grid.
    start = head [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, c == 'S']
    key (pos, steps) = pos
    actions (pos, steps) = [(pos + dv d, steps + 1) | d <- [U,D,L,R], Set.member (pos + dv d) floor]

    -- naively calculated solution on the infinite grid used for testing
    actions' (pos, steps) = [(pos + dv d, steps + 1) | d <- [U,D,L,R], Set.member (wrap (pos + dv d)) floor]
    ref = Set.size $ Set.fromList [pos | (pos,s) <- takeWhile ((<= target).snd) $ bfsOn key actions' (const True) (start, 0),
                                   mod s 2 == mod target 2]
