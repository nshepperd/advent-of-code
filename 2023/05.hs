{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Control.Lens
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

input, sample :: ([Int], [[[Int]]])
[input, sample] = fmap (check . parse p . unsafePerformIO . T.readFile) ["input/05.txt", "sample/05.txt"]
  where
    p = do
      seeds <- "seeds: " >> some (p_nat <* spaces)
      maps <- some $ do
        a <- letters
        "-to-"
        b <- letters
        " map:" >> spaces
        entries <- some $ replicateM 3 (p_nat <* spaces)
        return (a,b,entries)
      return (seeds, maps)
    check (seeds, maps)
      | head ax /= "seed" = error "seed"
      | last bx /= "location" = error "location"
      | any id (zipWith (/=) bx (tail ax)) = error "zip"
      | otherwise = (seeds, map (view _3) maps)
      where
        ax = map (view _1) maps
        bx = map (view _2) maps

pairs (x:y:xs) = (x,y):pairs xs
pairs [] = []

part1 (seeds, maps) = minimum (map (go maps) seeds)
  where
    go [] x = x
    go (m:maps) x = let y = head $ [d + (x - s) | [d,s,r] <- m, s <= x && x < s + r] ++ [x]
                    in go maps y

part2 :: ([Int], [[[Int]]]) -> Int
part2 (seeds, maps) = foldr go finish maps (pairs seeds)
  where
    finish xls = minimum [x | (x,l) <- xls]
    go ms k xls = k (foldr mgo mfinish ms xls)

    mfinish xls = xls
    mgo m k xls = case foldMap (map1 m) xls of
                    (ds, xls) -> ds ++ k xls

    map1 [d,s,r] (x,l)
      | ei > si = ([(di, ri)], [(y,w) | (y,w) <- [(x,si-x), (ei, x+l-ei)], w >= 1])
      | otherwise = ([], [(x,l)])
      where
        si = max s x
        ei = min (s + r) (x + l)
        di = d + si - s
        ri = ei - si

main = print (part2 input)
