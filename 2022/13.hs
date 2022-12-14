{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

data L = I Int | L [L]
  deriving (Show, Eq)

instance Ord L where
  (I a) <= (I b) = a <= b
  (L a) <= (L b) = a <= b
  (I a) <= b = L [I a] <= b
  a <= (I b) = a <= L [I b]

paragraphs :: Text -> [[Text]]
paragraphs txt = map T.lines (T.splitOn "\n\n" txt)

input, sample :: [[L]]
[input, sample] = fmap (map (map (parse p)) . paragraphs . unsafePerformIO . T.readFile) ["input/13.txt", "sample/13.txt"]
  where
    p = p_list

p_l = p_list <|> (I <$> p_int)
p_list = char '[' *> (L <$> (p_l `sepBy` (char ','))) <* char ']'

part1 input = [i | (i, [a,b]) <- zip [1..] input, a < b]

part2 input = product [i | (i, packet) <- zip [1..] (sort (concat input ++ extra)), packet `elem` extra]
  where
    extra = map (parse p_l) ["[[2]]", "[[6]]"]
