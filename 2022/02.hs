import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Debug.Trace
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

input :: [(Char, Char)]
input = unsafePerformIO (parse p <$> T.readFile "input/02.txt")
  where
    p = some $ do
      a <- oneOf "ABC"
      spaces
      b <- oneOf "XYZ"
      spaces
      return (a,b)

beats 'P' 'R' = True
beats 'S' 'P' = True
beats 'R' 'S' = True
beats _ _ = False

score_ (x, y) = innate + win
  where
    innate = case y of
               'R' -> 1
               'P' -> 2
               'S' -> 3
    win
      | beats y x = 6
      | beats x y = 0
      | otherwise = 3

score1 :: (Char, Char) -> Int
score1 = score_ . over each tr
  where
    tr 'A' = 'R'
    tr 'B' = 'P'
    tr 'C' = 'S'
    tr 'X' = 'R'
    tr 'Y' = 'P'
    tr 'Z' = 'S'

score2 :: (Char, Char) -> Int
score2 = score_ . tr'
  where
    tr' (x, 'X') = (tr x, head [y | y <- "RPS", tr x `beats` y])
    tr' (x, 'Y') = (tr x, tr x)
    tr' (x, 'Z') = (tr x, head [y | y <- "RPS", y `beats` tr x])
    tr 'A' = 'R'
    tr 'B' = 'P'
    tr 'C' = 'S'

main = do
  print (sum $ map score1 input)
  print (sum $ map score2 input)
