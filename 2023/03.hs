{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Util

input, sample :: [String]
[input, sample] = fmap (lines . unsafePerformIO . readFile) ["input/03.txt", "sample/03.txt"]

groupOn :: Eq a => (t -> a) -> [t] -> [[t]]
groupOn f = groupBy (\a b -> f a == f b)

neighbors (x,y) = [(x2,y2) | y2<- [y-1,y,y+1], x2 <- [x-1,x,x+1]]

part1 :: [String] -> Int
part1 input = sum [n | (ps, n) <- numbers, any (`elem` symbols) (ps >>= neighbors)]
  where
    symbols = [(x, y) | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, not (isDigit c) && c /= '.']
    numbers = [let (xs, ds) = unzip part
               in (map (\x -> (x,y)) xs, read ds :: Int)
              | (y,line) <- zip [0..] input, part <- groupOn (isDigit.snd) (zip [0..] line), all (isDigit.snd) part]

part2 :: [String] -> Int
part2 input = sum [product ns | ns <- Map.elems gearnums, length ns == 2]
  where
    gears = [(x, y) | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line, c == '*']
    numbers = [let (xs, ds) = unzip part
               in (map (\x -> (x,y)) xs, read ds :: Int)
              | (y,line) <- zip [0..] input, part <- groupOn (isDigit.snd) (zip [0..] line), all (isDigit.snd) part]
    gearnums = Map.fromListWith (++) [(g, [n]) | (ps, n) <- numbers, g <- nub (ps >>= neighbors), elem g gears]
