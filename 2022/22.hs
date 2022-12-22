{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Codensity
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Word
import           Debug.Trace
import           Linear.V2
import           Linear.V3
import           Linear
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util


type Pos = V2 Int
input, sample :: (Map Pos Char, [Either Int Char])
[input, sample] = fmap (f . paragraphs . unsafePerformIO . T.readFile) ["input/22.txt", "sample/22.txt"]
  where
    f [grid, [path]] = (Map.fromList [(V2 x y, c) | (y, line) <- zip [0..] grid, (x, c) <- zip [0..] (T.unpack line), elem c ['.', '#']],
                      parse p path)
    p = some ((Left <$> p_nat) <|> (Right <$> oneOf "LR"))

fp (V2 a b) = V2 b a

part1 (grid, path) = foldr go finish path (start, 0)
  where
    d 0 = V2 1 0
    d 1 = V2 0 1
    d 2 = V2 (-1) 0
    d 3 = V2 0 (-1)
    start = fp . minimum . map fp $ Map.keys grid
    go (Left n) k (pos, face) = k (iterate step pos !! n, face)
      where
        wrap pos | Map.member (pos + d face) grid = pos + d face
                 | otherwise = last $ takeWhile (flip Map.member grid) $ iterate (\p -> p - d face) pos
        step pos = case Map.lookup (wrap pos) grid of
           Just '.' -> wrap pos
           Just '#' -> pos
    go (Right 'L') k (pos, face) = k (pos, mod (face - 1) 4)
    go (Right 'R') k (pos, face) = k (pos, mod (face + 1) 4)
    finish (V2 x y, face) = 1000 * (y+1) + 4 * (x+1) + face

data CubePos = CP (V3 Int) (V3 Int)
  deriving (Show, Eq, Ord)

center :: V2 Int -> Int -> V2 Int
center p tsize = let y = p - root p - pure (div tsize 2) in y + (max 0 . signum . (+1) <$> y)
  where
    root (V2 x y) = V2 (x - mod x tsize) (y - mod y tsize)

uncenter :: V2 Int -> Int -> V2 Int
uncenter p tsize = f <$> p
  where
    f x | x < 0 = x + div tsize 2
        | x > 0 = x + div tsize 2 - 1

rotation :: V3 Int -> V3 Int -> V3 Int
rotation (V3 1 0 0) (V3 x y z) = V3 x (-z) y
rotation (V3 (-1) 0 0) (V3 x y z) = V3 x z (-y)
rotation (V3 0 1 0) (V3 x y z) = V3 z y (-x)
rotation (V3 0 (-1) 0) (V3 x y z) = V3 (-z) y x
rotation (V3 0 0 1) (V3 x y z) = V3 (-y) x z
rotation (V3 0 0 (-1)) (V3 x y z) = V3 y (-x) z

part2 (grid, path) tsize = foldr go finish path (startv2, 0)
  where
    d 0 = V2 1 0
    d 1 = V2 0 1
    d 2 = V2 (-1) 0
    d 3 = V2 0 (-1)
    start = fp . minimum . map fp $ Map.keys grid
    go (Left n) k (pos, face) = k (iterate step (pos, face) !! n)
      where
        step (pos,face) = case Map.lookup (fst $ wrap (pos, d face)) grid of
           Just '.' -> case wrap (pos, d face) of
             (pos', V2 1 0) -> (pos', 0)
             (pos', V2 0 1) -> (pos', 1)
             (pos', V2 (-1) 0) -> (pos', 2)
             (pos', V2 0 (-1)) -> (pos', 3)
           Just '#' -> (pos, face)
    go (Right 'L') k (pos, face) = k (pos, mod (face - 1) 4)
    go (Right 'R') k (pos, face) = k (pos, mod (face + 1) 4)
    finish (V2 x y, face) = 1000 * (y+1) + 4 * (x+1) + face


    half = div tsize 2
    root (V2 x y) = V2 (x - mod x tsize) (y - mod y tsize)

    to3d :: (V3 Int,V3 Int,V3 Int) -> V2 Int -> V3 Int
    to3d (ex, ey, ez) p = case center p tsize of
      V2 x y -> ex * pure x + ey * pure y + ez * pure half
    from3d (ex, ey, ez) p = uncenter (V2 (sum (ex * p)) (sum (ey * p))) tsize

    wrap (p, d)
      | Map.member (p + d) grid = (p + d, d)
      | otherwise = (newp, newd)
      where
        (ex, ey, ez) = rootmap Map.! root p
        ez' = rotatez d (ex,ey)
        (ex', ey') = z_to_coords Map.! ez'
        newroot = z_to_root Map.! ez'
        newp = newroot + from3d (ex', ey', ez') (to3d (ex,ey,ez) p)

        rot = case d of
          V2 1 0 -> rotation (-ey)
          V2 (-1) 0 -> rotation ey
          V2 0 1 -> rotation ex
          V2 0 (-1) -> rotation (-ex)

        d3 = d *! V2 ex ey
        newd3 = rot d3
        newd = V2 (sum (newd3 * ex')) (sum (newd3 * ey'))

    z_to_coords = Map.fromList [(ez, (ex, ey)) | (ex, ey, ez) <- toList rootmap]
    z_to_root = Map.fromList [(ez, r) | (r, (ex, ey, ez)) <- Map.toList rootmap]
    root_to_z = (\(ex,ey,ez) -> ez) <$> rootmap
    rootmap = dfs [(startv2, (V3 1 0 0, V3 0 (-1) 0, V3 0 0 1))] Map.empty

    rotatez d (ex,ey) = case signum d of
      V2    1    0 -> ex
      V2 (-1)    0 -> -ex
      V2    0    1 -> ey
      V2    0 (-1) -> -ey

    startv2 = fp . minimum . map fp $ Map.keys grid
    dfs [] visited = visited
    dfs ((p2, (ex,ey,ez)):frontier) visited
      | Map.member p2 visited = dfs frontier visited
      | otherwise = dfs (neighbors ++ frontier) visited'
      where
        neighbors = [(p2 + del, solve (p2 + del)) | del <- [V2 tsize 0, V2 0 tsize, V2 (-tsize) 0, V2 0 (-tsize)], Map.member (p2 + del) grid]
        visited' = Map.insert p2 (ex,ey,ez) visited
        solve n = rotate (n - p2) (ex,ey,ez)
        rotate d (ex,ey,ez) = case signum d of
          V2    1    0 -> (-ez, ey, ex)
          V2 (-1)    0 -> (ez, ey, -ex)
          V2    0    1 -> (ex, -ez, ey)
          V2    0 (-1) -> (ex, ez, -ey)
