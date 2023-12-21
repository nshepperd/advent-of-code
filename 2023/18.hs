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


input, sample :: [(Dir, Int, String)]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/18.txt", "sample/18.txt"]
  where
    p = some $ do
      d <- read . pure <$> letter <* spaces
      n <- p_nat <* spaces
      str <- "(#" *> alnums <* ") "
      return (d, n, T.unpack str)

data Dir = U | D | L | R
  deriving (Show, Eq, Ord, Read)

dv :: Dir -> V2 Int
dv U = -V2 0 1
dv D = V2 0 1
dv L = -V2 1 0
dv R = V2 1 0

part1 input = foldr go finish input 0 0 0
  where
    go (d, n, str) k pos area len
      | d == L = k pos' (area - n * y) len'
      | d == R = k pos' (area + n * y) len'
      | otherwise = k pos' area len'
      where
        y = view _y pos
        pos' = pos + dv d ^* n
        len' = len + n
    finish pos area len = let interior = abs area - (len`div`2) + 1
                               -- pick's theorem!
                          in interior + len

fromhex str = foldr go 0 (reverse str)
  where
    go c r | isDigit c = read [c] + 16 * r
           | otherwise = (ord c - ord 'a' + 10) + 16 * r

part2 input = part1 [([R,D,L,U]!!read [last str], fromhex (init str), str)  | (_, _, str) <- input]
