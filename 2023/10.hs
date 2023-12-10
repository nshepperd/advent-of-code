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
import           Debug.Trace
import           Linear.V2
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [[String]]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/10.txt", "sample/10.txt"]
  where
    p = some (p_puz <* optional (char '\n'))
    p_puz = some (some (oneOf "F7JL|-.S") <* optional (char '\n'))

data Direction = N | E | S | W | C
  deriving (Show,Eq,Ord)

dv :: Direction -> V2 Int
dv N = -V2 0 1
dv S = V2 0 1
dv E = V2 1 0
dv W = -V2 1 0
dv C = V2 0 0

part1 input = maximum $ map snd $ bfsOn fst actions (const True) start
  where
    grid = Map.fromList [(V2 x y,c) | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line]
    gridc p = Map.findWithDefault '.' p grid
    start = head [(p,0) | (p,'S') <- Map.toList grid]
    actions (p,cost) = [(p + v, cost+1) | d <- sides (gridc p), d /= C, let v = dv d, elem (-v) (map dv $ sides $ gridc $ p + v)]

sides :: Char -> [Direction]
sides 'J' = [N, W, C]
sides 'L' = [N, E, C]
sides 'F' = [S, E, C]
sides '7' = [S, W, C]
sides '|' = [N, C, S]
sides '-' = [W, C, E]
sides 'S' = [N,E,S,W,C]
sides '.' = []

draw :: Char -> Char
draw 'J' = '┘'
draw 'F' = '┌'
draw '7' = '┐'
draw 'L' = '└'
draw '|' = '│'
draw '-' = '─'
draw c = c

part2 input = drawAnswer >> print (Set.size inside)
  where
    drawAnswer = putStr $ unlines [concat [
                                      (case () of
                                          _ | Set.member (V2 x y) inside -> "\x1b[32m"++ [draw c] ++ "\x1b[m"
                                            | Set.member (V2 x y) loop -> [draw c]
                                            | otherwise -> "\x1b[31m" ++ [draw c] ++ "\x1b[m")
                                      | (x,c) <- zip [0..] line] | (y,line) <- zip [0..] input]

    inside = Set.difference (Map.keysSet grid) (outside <> loop)
    outside = Set.fromList [p | mp <- bfsOn id actions (const True) (-1), let (p,dp) = untriple mp]
      where
        triple p = 3 * p + 1
        untriple mp = (fmap (`div` 3) mp, fmap (\a -> mod a 3 - 1) mp)
        inbounds (V2 x y) = (-1) <= x && x <= maxX + 1 && (-1) <= y && y <= maxY + 1
        walls = Set.fromList [triple p + dv d | p <- Set.toList loop, d <- sides (gridc p)]
        -- walls = Set.fromList [triple p + dv d | p <- Map.keys grid, d <- sides (gridc p)]
        allowed mp = inbounds (fst $ untriple mp) && not (Set.member mp walls)
        actions p = [p + d | d <- [V2 1 0, -V2 1 0, V2 0 1, -V2 0 1], allowed (p + d)]

    maxX = length (head input)
    maxY = length input
    grid = Map.fromList [(V2 x y, c) | (y,line) <- zip [0..] input, (x,c) <- zip [0..] line]
    gridc p = Map.findWithDefault '.' p grid
    start = head [p | (p,'S') <- Map.toList grid]
    loop = Set.fromList $ bfsOn id actions (const True) start
      where
        actions p = [p + v | d <- sides (gridc p), d /= C, let v = dv d, elem (-v) (map dv $ sides $ gridc $ p + v)]

main = traverse_ part2 input
