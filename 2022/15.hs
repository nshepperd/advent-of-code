{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import           Linear.V2
import           Linear.Matrix as M
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

input, sample :: [(V2 Int, V2 Int)]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/15.txt", "sample/15.txt"]
  where
    xy = do
      text "x="
      x <- p_int
      text ", y="
      y <- p_int
      return (V2 x y)
    p = some $ do
      text "Sensor at "
      a <- xy
      text ": closest beacon is at "
      b <- xy
      spaces
      return (a,b)

part1 input y = Set.size (foldMap part input `Set.difference` (Set.fromList [beac | (sens, beac) <- input]))
  where
    part (sens@(V2 sx sy), beac)
      | w > 0 = Set.fromList [V2 x y | x <- [sx-w`div`2..sx+w`div`2]]
      | w <= 0 = Set.empty
      where
        n = sum (abs <$> (beac - sens))
        w = 2 * (n - abs (sy - y)) + 1

-- |x - x1| + |y-y1| = d1+1
-- |x - x2| + |y-y2| = d2+1

data Line = Line (V2 Int) (V2 Int)
  deriving Show

lorig :: Line -> V2 Int
lorig (Line s e) = s
ldir :: Line -> V2 Int
ldir (Line s e) = signum <$> (e - s)

-- ax - bx + t dax = s dbx
-- ay - by + t day = s dby

-- ax - bx = s dbx - t dax
-- ay - by = s dby - t day

-- [ax - bx] = [dbx  -dax] [s]
-- [ay - by] = [dby  -day] [t]

iinv :: M22 Int -> M22 Int
iinv (V2 (V2 a b) (V2 c d)) = V2 (V2 d (-b)) (V2 (-c) a)

lintersect :: Line -> Line -> [V2 Int]
lintersect a b = case det22 mat of
  0 -> []
  n -> case iinv mat !* (lorig a - lorig b) of
    V2 ds dt | dt `mod` n == 0 -> [lorig a + pure (dt `div` n) * ldir a]
             | otherwise -> [lorig a + let frac = pure (fromIntegral dt / fromIntegral n) * (fromIntegral <$> ldir a)
                                       in rd <*> frac |
                              rd <- sequence (pure [floor, ceiling])
                            ]
  where
    mat = M.transpose (V2 (ldir b) (-ldir a))

md p q = sum (abs <$> (p - q))

part2 input = case head (filter check candidates) of
  V2 x y -> x * 4000000 + y
  where
    check p = and $ [md sens beac < md sens p | (sens,beac) <- input]
    edges (sens1, beac1) = [Line (sens1 + V2 d 0) (sens1 + V2 0 d),
                            Line (sens1 + V2 0 d) (sens1 + V2 (-d) 0),
                            Line (sens1 + V2 (-d) 0) (sens1 + V2 0 (-d)),
                            Line (sens1 + V2 0 (-d)) (sens1 + V2 d 0)]
      where
        n = sum (abs <$> (beac1 - sens1))
        d = n + 1
    inrange p = 0 <= minimum p && maximum p <= 4000000
    candidates = Set.toList . Set.fromList  $ filter inrange $ foldMap go [(a, b) | a <- input, b <- input, b /= a]
    go ((sens1, beac1),(sens2, beac2)) =
      fold [lintersect e1 e2 | e1 <- edges (sens1, beac1), e2 <- edges (sens2, beac2)]

main :: IO ()
main = do
  print (part2 input)
