import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Linear
import           System.IO
import           System.IO.Unsafe

import           AStar
import           Util

input, sample :: [String]
[input, sample] = fmap (lines . unsafePerformIO . readFile) ["input/24.txt", "sample/24.txt"]

type Pos = V2 Int

neighbors p = [p + d | d <- basis ++ map negate basis]

d '>' = V2 1 0
d '<' = V2 (-1) 0
d 'v' = V2 0 1
d '^' = V2 0 (-1)

part1 input = astar actions step goal (start,0) output
  where
    draw blizz e = putStr $ unlines [[drawp (V2 x y) | x <- [xmin-1..xmax+1]] | y <- [ymin-1..ymax+1]]
      where
        drawp (V2 x y) | input!!y!!x == '#' = '#'
                       | (V2 x y) == e = 'E'
                       | length bz == 1 = head bz
                       | length bz > 1 = 'x'
                       | otherwise = '.'
          where bz = [c | (c, m) <- Map.toList blizz, Set.member (V2 x y) m]

    start = head [V2 x y | (y,line) <- zip [0..] input, (x, '.') <- zip [0..] line]
    end = last [V2 x y | (y,line) <- zip [0..] input, (x, '.') <- zip [0..] line]

    xmin = 1 :: Int
    ymin = 1 :: Int
    xmax = length (head input) - 2 :: Int
    ymax = length input - 2 :: Int

    blizzL = iterate advance blizz

    wrap (V2 x y) = V2 (xmin + mod (x - xmin) (xmax - xmin + 1)) (ymin + mod (y - ymin) (ymax - ymin + 1))

    blizz = Map.fromListWith (<>) [(c, Set.singleton (V2 x y)) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, elem c ("><^v"::String)] :: Map Char (Set Pos)
    advance = Map.mapWithKey $ \c blizz ->
      Set.map (\p -> wrap (p + d c)) blizz

    valid p@(V2 x y) | p == end = True
                     | p == start = True
                     | x < xmin = False
                     | x > xmax = False
                     | y < ymin = False
                     | y > ymax = False
                     | otherwise = True


    actions (p,t) = [(n,t+1) | n <- neighbors p ++ [p], not $ any (Set.member n) blizz', valid n]
      where
        blizz' = blizzL !! (t+1)

    step s a = (a, 1)
    goal (p,t) = sum (abs <$> (p - end))
    output s a n c = Sum c


part2 input = astar actions step goal (start,0,[end,start,end]) output
  where
    draw blizz e = putStr $ unlines [[drawp (V2 x y) | x <- [xmin-1..xmax+1]] | y <- [ymin-1..ymax+1]]
      where
        drawp (V2 x y) | input!!y!!x == '#' = '#'
                       | (V2 x y) == e = 'E'
                       | length bz == 1 = head bz
                       | length bz > 1 = 'x'
                       | otherwise = '.'
          where bz = [c | (c, m) <- Map.toList blizz, Set.member (V2 x y) m]

    start = head [V2 x y | (y,line) <- zip [0..] input, (x, '.') <- zip [0..] line]
    end = last [V2 x y | (y,line) <- zip [0..] input, (x, '.') <- zip [0..] line]

    xmin = 1 :: Int
    ymin = 1 :: Int
    xmax = length (head input) - 2 :: Int
    ymax = length input - 2 :: Int

    blizzL = iterate advance blizz

    wrap (V2 x y) = V2 (xmin + mod (x - xmin) (xmax - xmin + 1)) (ymin + mod (y - ymin) (ymax - ymin + 1))

    blizz = Map.fromListWith (<>) [(c, Set.singleton (V2 x y)) | (y, line) <- zip [0..] input, (x, c) <- zip [0..] line, elem c ("><^v"::String)] :: Map Char (Set Pos)
    advance = Map.mapWithKey $ \c blizz ->
      Set.map (\p -> wrap (p + d c)) blizz

    valid p@(V2 x y) | p == end = True
                     | p == start = True
                     | x < xmin = False
                     | x > xmax = False
                     | y < ymin = False
                     | y > ymax = False
                     | otherwise = True


    actions (p,t,[]) = []
    actions (p,t,g:goals) = [(n,t+1,if g == n then goals else g:goals) |
                             n <- neighbors p ++ [p], not $ any (Set.member n) blizz', valid n]
      where
        blizz' = blizzL !! (t+1)

    step s a = (a, 1)
    goal (p,t,[]) = 0
    goal (p,t,gs) = sum $ zipWith (\a b -> sum (abs <$> (a - b))) (p:gs) gs
    output s a n c = Sum c
