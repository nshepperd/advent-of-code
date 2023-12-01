import Data.Char
import Data.List
import System.IO.Unsafe

input, sample :: [String]
[input, sample, sample2] = fmap (lines . unsafePerformIO . readFile) ["input/01.txt", "sample/01.txt", "sample/01-2.txt"]

part1 :: [String] -> Int
part1 input = sum (map go input)
  where
    go line = let ds = filter isDigit line
              in read [head ds, last ds]

part2 :: [String] -> Int
part2 input = sum $ map go input
  where
    patterns = zip (map pure ['1'..'9'] ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]) (cycle ['1'..'9'])
    search patterns str = [n | sub <- tails str, (pat,n) <- patterns, take (length pat) sub == pat]
    go line = let ds = search patterns line
              in read [head ds, last ds]
