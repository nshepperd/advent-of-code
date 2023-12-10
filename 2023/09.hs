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
import qualified Data.Vector as V

import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [[Int]]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/09.txt", "sample/09.txt"]
  where
    p = some $ do
      some (p_int <* many (char ' ')) <* spaces

-- The atzec god of consecutive numbers.
pairs :: [a] -> [(a,a)]
pairs = zip`ap`tail

part1 input = sum $ map solve1 input
  where
    solve1 xs | all (==0) xs = 0
    solve1 xs = last xs + solve1 [b-a | (a,b) <- pairs xs]

part2 input = sum $ map solve1 input
  where
    solve1 xs | all (==0) xs = 0
    solve1 xs = head xs - solve1 [b-a | (a,b) <- pairs xs]
