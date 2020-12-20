{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Debug.Trace
import qualified Ersatz as E
import qualified Ersatz.Counting as E
import qualified Ersatz.Solver.Minisat as E
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: Map Int [[Char]]
input = unsafePerformIO (parse p <$> T.readFile "input/20.txt")
  where
    p = fmap Map.fromList $ some $ do
      text "Tile "
      tid <- p_nat
      text ":\n"
      tile <- some $ do
        some (oneOf ".#") <* spaces
      return (tid, tile)

tileWidth = length . head $ head (toList input)
tileHeight = length $ head (toList input)

options' s = do
  xs <- sequence $ Map.fromSet (\_ -> E.exists :: Ersatz E.Bit) s
  E.assert (exactlyOne (toList xs))
  return xs

options n = do
  xs <- replicateM n E.exists :: Ersatz [E.Bit]
  E.assert (exactlyOne xs)
  return xs

type D8 = (Int,Int,Int)
d8 :: [D8]
d8 = [(a,b,c) | [a,b,c] <- replicateM 3 [0,1]]

runD8 :: D8 -> [[Char]] -> [[Char]]
runD8 (it, iv, ih) = apply it transpose . apply iv reverse . apply ih (map reverse)
  where
    apply 1 f = f
    apply 0 f = id

edges :: Set [Char]
edges = Set.fromList $ do
  tile <- toList input
  tr <- d8
  return (head (runD8 tr tile))

edgeID :: Map [Char] Int
edgeID = Map.fromList (zip (toList edges) [0..])
edgeIDs :: Set Int
edgeIDs = Set.fromList (toList edgeID)

data Side = N | E | S | W
  deriving (Show, Eq, Ord)

tileEdge :: Map (Int, D8, Side) Int
tileEdge = Map.fromList $ do
  (tid, tile) <- Map.toList input
  tr <- d8
  s <- [N, E, S, W]
  let tile' = runD8 tr tile
      edge = case s of
        N -> head
        E -> map last
        S -> last
        W -> map head
  return ((tid, tr, s), edgeID Map.! edge tile')

allowedWE :: Set ((Int,D8),(Int,D8))
allowedWE = Set.fromList $ do
  tid <- Map.keys input
  o <- d8
  tid2 <- Map.keys input
  o2 <- d8
  guard $ Map.lookup (tid, o, E) tileEdge == Map.lookup (tid2, o2, W) tileEdge
  return ((tid, o), (tid2, o2))

allowedNS :: Set ((Int,D8),(Int,D8))
allowedNS = Set.fromList $ do
  tid <- Map.keys input
  o <- d8
  tid2 <- Map.keys input
  o2 <- d8
  guard $ Map.lookup (tid, o, S) tileEdge == Map.lookup (tid2, o2, N) tileEdge
  return ((tid, o), (tid2, o2))

fromopts :: Map k Bool -> k
fromopts map = head [i | (i, True) <- Map.toList map]

part1 :: Map (Int,Int) (Int, D8)
part1 = final
  where
    gridW = 12
    gridH = 12
    final = case sol of
      (grid, orient) -> Map.intersectionWith (,) (fromopts <$> grid) (fromopts <$> orient)
    sol = head $ solves $ do
      let keys = [(x, y) | x <- [0..gridW-1], y <- [0..gridW-1]]
          keysSet = Set.fromList keys
      grid <- sequence $ Map.fromSet (\p -> options' (Map.keysSet input)) keysSet
      orient <- sequence $ Map.fromSet (\p -> options' (Set.fromList d8)) keysSet
      traverse_ (E.assert . exactlyOne) (transpose $ toList <$> toList grid)
      for_ keys $ \(x,y) -> do
        when (x + 1 < gridW) $ do
          let p = (x,y)
              p2 = (x+1, y)
          E.assert $ E.or [E.and [grid Map.! p Map.! tid, orient Map.! p Map.! o,
                                  grid Map.! p2 Map.! tid2, orient Map.! p2 Map.! o2]
                          | ((tid, o), (tid2, o2)) <- toList allowedWE]
        when (y + 1 < gridH) $ do
          let p = (x,y)
              p2 = (x,y+1)
          E.assert $ E.or [E.and [grid Map.! p  Map.! tid,  orient Map.! p  Map.! o,
                                  grid Map.! p2 Map.! tid2, orient Map.! p2 Map.! o2]
                          | ((tid, o), (tid2, o2)) <- toList allowedNS]
      return (grid, orient)

solved_part1 :: Map (Int,Int) (Int, D8)
solved_part1 = unsafePerformIO (read <$> readFile "output/20.txt")

part2 = head monsters
  where
    w = length (head grid)
    h = length grid
    grid = [[ let (tid, tr) = solved_part1 Map.! (x,y)
                  tile = input Map.! tid
              in runD8 tr tile !! dy !! dx
            | x <- [0..11], dx <- [1..8]]
           | y <- [0..11], dy <- [1..8]]
    monsters = do
      tr <- d8
      let grid' = runD8 tr grid
      let monsters = [(x, y) | x <- [0..w-mw], y <- [0..h-mh], matchMonster grid' x y]
      guard (length monsters > 0)
      let gridmap = Map.fromList [((x,y), grid' !! y !! x) | y <- [0..h-1], x <- [0..w-1]]
          cleared = Map.fromList [((x+dx,y+dy), '.') | (x,y) <- monsters, dy <- [0..mh-1], dx <- [0..mw-1], monster !! dy !! dx == '#']
          grid'' = cleared <> gridmap
      return (count '#' grid'')
    matchMonster grid x y = and [grid!!(y+dy)!!(x+dx) == '#' | dy <- [0..mh-1], dx <- [0..mw-1], monster !! dy !! dx == '#']
    monster = ["                  # ",
               "#    ##    ##    ###",
               " #  #  #  #  #  #   "]
    mw = length (head monster)
    mh = length monster

main :: IO ()
main = do
  let val = part1
  print val
  writeFile "output/20.txt" (show val)
