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

input, sample :: [String]
[input, sample] = fmap (lines . unsafePerformIO . readFile) ["input/11.txt", "sample/11.txt"]

solve r input = sum [sum (abs <$> (p - q)) | p:ps <- tails expanded, q <- ps]
  where
    galaxies = Set.fromList [V2 x y | (y,line) <- zip [0..] input, (x,'#') <- zip [0..] line]
    g_xs = Set.map (view _x) galaxies
    g_ys = Set.map (view _y) galaxies

    e_xs = Set.difference (Set.fromList [Set.findMin g_xs .. Set.findMax g_xs]) g_xs
    e_ys = Set.difference (Set.fromList [Set.findMin g_ys .. Set.findMax g_ys]) g_ys

    expanded = [V2 (x + (r-1)*Set.size (fst (Set.split x e_xs))) (y + (r-1)*Set.size (fst (Set.split y e_ys)))  | V2 x y <- Set.toList galaxies]

part1 = solve 2
part2 = solve 1000000
