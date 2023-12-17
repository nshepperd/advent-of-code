{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Linear.V2
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators (eof)
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [String]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/17.txt", "sample/17.txt"]
  where
    p = some $ some digit <* spaces

dv :: Char -> V2 Int
dv '^' = -V2 0 1
dv 'v' = V2 0 1
dv '<' = -V2 1 0
dv '>' = V2 1 0

data S = S {pos :: V2 Int,
            prev :: [Char],
            heat :: Int,
            path :: [Char]
            }
  deriving Show

part1 grid = heat $ head $ astarOn key actions goal start
  where
    xmax = length (grid !! 0)
    ymax = length grid
    inbounds (V2 x y) = 0 <= x && x < xmax && 0 <= y && y < ymax
    start = S 0 [] 0 []
    key (S pos prev _ _) = (pos, prev)
    actions (S pos prev heat path) = [(S npos nprev (heat + h) (d:path), h)
                                     | d <- "^v<>",
                                       let { npos = pos + dv d }, inbounds npos,
                                       let { nprev = grouped (d:prev) }, length nprev <= 3,
                                       let { h = gridp npos },
                                       prev == [] || dv d /= -dv (head prev)
                                     ]
    grouped prev = fold (take 1 (group prev))
    gridp (V2 x y) = read $ pure (grid!!y!!x)
    goal (S pos _ _ _) = sum $ abs <$> (V2 (xmax-1) (ymax-1) - pos)

part2 grid = heat $ head $ astarOn key actions goal start
  where
    xmax = length (grid !! 0)
    ymax = length grid
    inbounds (V2 x y) = 0 <= x && x < xmax && 0 <= y && y < ymax
    start = S 0 [] 0 []
    key (S pos prev _ _) = (pos, prev)
    actions (S pos prev heat path) = [(S npos nprev (heat + h) (d:path), h)
                                     | d <- "^v<>",
                                       let { npos = pos + dv d }, inbounds npos,
                                       let { nprev = grouped (d:prev) },
                                       let { h = gridp npos },
                                       case prev of
                                         [] -> True
                                         (p:_) | dv p + dv d == 0 -> False
                                         (p:_) | p /= d -> length (grouped prev) >= 4
                                         (p:_) | p == d -> length (grouped prev) <= 9
                                         _ -> True
                                     ]
    grouped prev = fold (take 1 (group prev))
    gridp (V2 x y) = read $ pure (grid!!y!!x)
    goal (S pos _ _ _) = sum $ abs <$> (V2 (xmax-1) (ymax-1) - pos)
