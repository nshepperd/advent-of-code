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
import           Text.Parser.Combinators (eof)
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [String]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/14.txt", "sample/14.txt"]
  where
    p = some $ some (oneOf ".#O") <* optional "\n"

part1 input = load $ transpose $ map roll (transpose input)
  where
    roll col = foldMap roll1 (groupBy (\a b -> b `elem` ['.','O']) col)
    roll1 col = replicate (n '#') '#' ++ replicate (n 'O') 'O' ++ replicate (n '.') '.'
      where
        counts = Map.fromListWith (+) [(c,1) | c <- col]
        n c = Map.findWithDefault 0 c counts
    load rows = sum [i * count 'O' row | (i,row) <- zip [1..] (reverse rows)]

part2 input = foldr go undefined (zip [0..] process) Map.empty
  where
    go (i,grid) k visited = case Map.lookup grid visited of
      Just j -> let s = mod (1000000000 - j) (i-j)
                in load (process !! (j + s))
      Nothing -> k (Map.insert grid i visited)
    process = iterate spin input
    spin = (
      map (reverse . roll . reverse) . -- E
      transpose . map (reverse . roll . reverse) . transpose . -- S
      map roll . -- W
      transpose . map roll . transpose -- N
      )
    roll col = foldMap roll1 (groupBy (\a b -> b `elem` ['.','O']) col)
    roll1 col = replicate (n '#') '#' ++ replicate (n 'O') 'O' ++ replicate (n '.') '.'
      where
        counts = Map.fromListWith (+) [(c,1) | c <- col]
        n c = Map.findWithDefault 0 c counts
    load rows = sum [i * count 'O' row | (i,row) <- zip [1..] (reverse rows)]
