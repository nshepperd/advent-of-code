{-# LANGUAGE OverloadedStrings #-}

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
import           Debug.Trace
import           Linear.V2
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [(String, [Int])]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/12.txt", "sample/12.txt"]
  where
    p = some $ do
      line <- some (oneOf "?.#") <* spaces
      nums <- p_nat `sepBy` "," <* spaces
      return (line, nums)

memo2 :: Int -> Int -> (Int -> Int -> a) -> (Int -> Int -> a)
memo2 m k f = \i j -> array V.! i V.! j
  where
    array = V.generate (m+1) (\i -> V.generate (k+1) (\j -> f i j))

solve line nums = go 0 0
  where
    m = length line
    k = length nums

    go = memo2 m k fgo
    fgo i j -- i: line index; j: nums index
      | i == m && j == k = 1
      | i == m           = 0
      |           j == k = if any (=='#') (drop i line) then 0 else 1
      | c == '.'         = go (i+1) j
      | c == '#'         = group i j
      | c == '?'         = group i j + go (i+1) j
      where
        c = line!!i
        n = nums!!j

    -- Check that we can fit a group starting here.
    group i j
      | any (=='.') sub = 0
      | (i+n) > m = 0
      | (i+n) < m && (line!!(i+n)) == '#' = 0
      | i+n == m = go (i+n) (j+1)
      | otherwise = go (i+n+1) (j+1)
      where
        n = nums!!j
        sub = take n (drop i line)

part1 input = sum [solve line nums | (line,nums) <- input]
part2 input = sum [solve (intercalate "?" (replicate 5 line)) (concat (replicate 5 nums)) | (line,nums) <- input]
