import           Control.Applicative
import           Data.List
import           System.IO.Unsafe

import           Util

input, sample :: [String]
[input, sample] = fmap (lines . unsafePerformIO . readFile) ["input/25.txt", "sample/25.txt"]

decode :: String -> Integer
decode cs = foldr go finish cs 0
  where
    finish x = x
    go c k x = k (x * 5 + dec c)
    dec '0' = 0
    dec '1' = 1
    dec '2' = 2
    dec '-' = -1
    dec '=' = -2

encode :: Integer -> String
encode n = foldr go finish (reverse [0..l-1]) []
  where
    finish prefix = prefix
    go r k prefix = k (prefix ++ [c])
      where
        c = head [c | c <- "=-012" :: String, decode (prefix ++ [c] ++ replicate r '2') >= n]
    l = head [l | l <- [0..], decode (replicate l '2') >= n]

part1 input = encode . sum . map decode $ input
