{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta
import           Debug.Trace

import           Util

instance a ~ Text => IsString (Parser a) where
  fromString str = text (T.pack str)

input, sample :: [([Int], [Int])]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/04.txt", "sample/04.txt"]
  where
    p = some $ do
      "Card" >> spaces >> p_nat >> ": " >> spaces
      win <- some (p_nat <* spaces)
      "|" >> spaces
      have <- some (p_nat <* spaces)
      return (win, have)

part1, part2 :: [([Int], [Int])] -> Int
part1 input = sum $ map go input
  where
    go (win, have) = case length [n | n <- have, elem n win] of
                       0 -> 0
                       n -> 2^(n-1)

part2 input = foldr go finish wins 0 []
  where
    wins = map calcwin input
    calcwin (win, have) = length [n | n <- have, elem n win]
    addzip [] ys = ys
    addzip xs [] = xs
    addzip (x:xs) (y:ys) = (x+y):addzip xs ys
    go w k total extra = let copies = 1 + head (extra ++ [0])
                             extra' = addzip (replicate w copies) (tail (extra ++ [0]))
                         in traceShow (w, copies, extra) $ k (total + copies) extra'
    finish total extra = total
