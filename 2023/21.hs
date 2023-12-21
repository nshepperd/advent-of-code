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

tri n = (n * (n+1)) `div` 2

-- sum [1+a | a <- [0..exw], a `mod` 2 == phase]
triphase exw phase = if phase == 0 then
                       -- sum [1+a | a <- [0..exw], a `mod` 2 == 0]
                       -- sum [1+a | a <- [0,2..exw]]
                       -- sum [1+2a | a <- [0,1..exw/2]]
                       -- (1+exw/2) + 2 sum [a | a <- [0,1..exw/2]]
                       -- (1+exw/2) + 2 tri (exw/2)
                       let a = (exw`div`2) in (1 + a + 2 * tri a)
                     else
                       -- sum [1+a | a <- [0..exw], a `mod` 2 == 1]
                       -- sum [1+1+a | a <- [0,2..exw-1]]
                       -- sum [2+2a | a <- [0,1..(exw-1)/2]]
                       let a = (exw-1)`div`2 in 2*(1+a + tri a)


part2 input = (fast, sum fast, ref, width, height)
  where
    fast = map sum (
      [[1 | (pos,s) <- Map.toList (pre Map.! start), s <= target, mod s 2 == mod target 2]] ++
      [[calculate pos s anchor | (pos,s) <- Map.toList dists]
      | (anchor, dists) <- Map.toList pre, anchor /= start])
    calculate pos s anchor
      | sum (fmap (abs.signum) (anchor-start)) == 2 = -- corner
        triphase exw phase
      | sum (fmap (abs.signum) (anchor-start)) == 1 = -- edge
        if phase == 0 then
          -- sum [1 | a <- [0..exw], a `mod` 2 == 0]
          -- sum [1 | a <- [0,2..exw]]
          -- sum [1 | a <- [0,1..exw/2]]
          -- 1 + exw/2
          1 + div exw 2
        else
          -- sum [1 | a <- [0..exw], a `mod` 2 == 1]
          -- sum [1 | a <- [1,3..exw]]
          -- sum [1 | a <- [0,1..(exw-1)/2]]
          1 + div (exw-1) 2
        -- sum [1 | a <- [0..exw], a `mod` 2 == phase]
      where
        s1 = sum (fmap (abs.signum) (anchor-start)) * (div width 2 + 1)
        s3 = s
        ex = target - s1 - s3
        phase = ex `mod` 2
        exw = ex `div` size
    size = width
    !_ = if width /= height then error "aa" else ()
    !_ = if size `mod` 2 /= 1 then error "bb" else ()
    ref = Set.size $ Set.fromList [pos | (pos,s) <- takeWhile ((<= target).snd) $ bfsOn key actions' (const True) (start, 0),
                                   mod s 2 == mod target 2]

    target = 26501365
    pre = Map.fromList [(V2 x y, Map.fromList $ bfsOn key actions (const True) (V2 x y, 0)) | x <- [0, sx, width-1], y <- [0, sy, height-1]]
    height = length input
    width = length (head input)
    ymax = height-1
    xmax = width-1
    wrap pos = mod <$> pos <*> V2 width height
    start = head [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, c == 'S']
    V2 sx sy = start
    floor = Set.fromList [V2 x y | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, elem c ['S','.']]
    key (pos, steps) = pos
    actions (pos, steps) = [(pos + dv d, steps + 1) | d <- [U,D,L,R], Set.member (pos + dv d) floor]
    actions' (pos, steps) = [(pos + dv d, steps + 1) | d <- [U,D,L,R], Set.member (wrap (pos + dv d)) floor]
