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
import           Numeric.Search.Range

import           Util

instance a ~ Text => IsString (Parser a) where
  fromString str = text (T.pack str)

input, sample :: [(String,Int)]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/07.txt", "sample/07.txt"]
  where
    p = some $ do
      hand <- replicateM 5 alphaNum
      " "
      bid <- p_nat
      spaces
      return (hand, bid)

type Hand = String

part1 input = sum $ zipWith (\(hand,bid) rank -> bid * rank) hands [1..]
  where
    hands = sortOn (score.fst) input

    score :: Hand -> ([Int],[Int])
    score hand = (handType, vs)
      where
        vs = map value hand
        handType = reverse $ sort $ Map.elems $ Map.fromListWith (+) [(c,1) | c <- hand]

    value :: Char -> Int
    value c | isDigit c = read (pure c)
    value 'T' = 10
    value 'J' = 11
    value 'Q' = 12
    value 'K' = 13
    value 'A' = 14

part2 input = sum $ zipWith (\(hand,bid) rank -> bid * rank) hands [1..]
  where
    hands = sortOn (score.fst) input

    score :: Hand -> ([Int],[Int])
    score hand = (handType, vs)
      where
        vs = map value hand
        handSubbed joker = reverse $ sort $ Map.elems $ Map.fromListWith (+) [(if c == 'J' then joker else c,1) | c <- hand]
        handType = maximum [handSubbed c | c <- "23456789TKQA"]

    value :: Char -> Int
    value c | isDigit c = read (pure c)
    value 'T' = 10
    value 'J' = 1
    value 'Q' = 12
    value 'K' = 13
    value 'A' = 14
