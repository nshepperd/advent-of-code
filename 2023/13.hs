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

input, sample :: [[String]]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/13.txt", "sample/13.txt"]
  where
    p = p_pat `sepBy` "\n"
    p_pat = some $ some (oneOf ".#") <* optional "\n"

splits :: [a] -> [([a],[a])]
splits (x:xs) = go [x] xs
  where
    go xs [] = []
    go xs (y:ys) = (xs, y:ys) : go (y:xs) ys

reflect1 xs = sum [length a | (a,b) <- splits xs, all id (zipWith (==) a b)]
reflect2 xs = sum [length a | (a,b) <- splits xs, 1 == length (filter id (zipWith (/=) (concat a) (concat b)))]

solve reflect grid = 100 * reflect rows + reflect cols
  where
    rows = grid
    cols = transpose grid

part1 input = sum $ map (solve reflect1) input
part2 input = sum $ map (solve reflect2) input
