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

input, sample :: [(Int,Int)]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/06.txt", "sample/06.txt"]
  where
    p = do
      times <- "Time:" >> spaces >> some (p_nat <* spaces)
      distance <- "Distance:" >> spaces >> some (p_nat <* spaces)
      return (zip times distance)

part1 input = product (map solve input)
  where
    -- d = hold * (time - hold)
    -- 0 = hold^2 - time * hold + d
    -- hold = (time +- sqrt(time^2 - 4 d)) / 2
    solve (time, distance) = h2 - h1 + 1
      where
        p hold = hold * (time - hold) > distance
        tf = fromIntegral time
        df = fromIntegral distance
        hf1 = (tf - sqrt (tf^2 - 4*df)) / 2
        hf2 = (tf + sqrt (tf^2 - 4*df)) / 2

        h1 = head [hold | dt <- [-2..2], let hold = round hf1 + dt, p hold]
        h2 = last [hold | dt <- [-2..2], let hold = round hf2 + dt, p hold]

part2 input = part1 $ pure $ over each (read . concat . map show :: [Int] -> Int) $ unzip input
