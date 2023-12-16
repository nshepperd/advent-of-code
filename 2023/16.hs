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
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/16.txt", "sample/16.txt"]
  where
    p = some $ some (oneOf ".-|/\\") <* spaces

dv :: Char -> V2 Int
dv '^' = -V2 0 1
dv 'v' = V2 0 1
dv '<' = -V2 1 0
dv '>' = V2 1 0

elemc :: Char -> String -> Bool
elemc = elem

solve grid start = go Set.empty [start]
  where
    width = length (grid !! 0)
    height = length grid
    inbounds (V2 x y) = 0 <= x && x < width && 0 <= y && y < height
    go visited [] = Set.size $ Set.map fst visited
    go visited ((p@(V2 x y), d) : frontier)
      | not (inbounds p) = go visited frontier
      | Set.member (p,d) visited = go visited frontier
      | otherwise = case [c, d] of
          ['.', d] -> continue [d]
          ['|', d] | d `elemc` "><" -> continue "^v"
          ['|', d] | d `elemc` "^v" -> continue [d]
          ['-', d] | d `elemc` "><" -> continue [d]
          ['-', d] | d `elemc` "^v" -> continue "><"
          "\\>" -> continue "v"
          "\\<" -> continue "^"
          "\\v" -> continue ">"
          "\\^" -> continue "<"

          "/>" -> continue "^"
          "/<" -> continue "v"
          "/v" -> continue "<"
          "/^" -> continue ">"
      where
        c = grid !! y !! x
        visited' = Set.insert (p,d) visited
        continue ds = go visited' ([(p + dv d, d) | d <- ds] ++ frontier)

part1 grid = solve grid (0, '>')
part2 grid = maximum (map (solve grid) starts)
  where
    starts = [(V2 0 y, '>') | y <- [0..height-1]] ++
             [(V2 x 0, 'v') | x <- [0..width-1]] ++
             [(V2 (width-1) y, '<') | y <- [0..height-1]] ++
             [(V2 x (height-1), '^') | x <- [0..width-1]]
    width = length (grid !! 0)
    height = length grid
