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
import           Data.Bifunctor
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

newtype EdgeId = EdgeId Int
  deriving (Eq, Ord, Show)

newtype TileId = TileId Int
  deriving (Eq, Ord, Show)

data Side = N | E | S | W
  deriving (Show, Eq, Ord)

type D8 = (Int,Int,Int)

d8 :: [D8]
d8 = [(a,b,c) | [a,b,c] <- replicateM 3 [0,1]]

runD8 :: D8 -> [[Char]] -> [[Char]]
runD8 (it, iv, ih) = apply it transpose . apply iv reverse . apply ih (map reverse)
  where
    apply 1 f = f
    apply 0 f = id

input :: Map TileId [[Char]]
input = unsafePerformIO (parse p <$> T.readFile "input/20.txt")
  where
    p = fmap Map.fromList $ some $ do
      text "Tile "
      tid <- TileId <$> p_nat
      text ":\n"
      tile <- some $ do
        some (oneOf ".#") <* spaces
      return (tid, tile)

tileWidth = length . head $ head (toList input)
tileHeight = length $ head (toList input)

getEdge :: [[Char]] -> Side -> [Char]
getEdge tile N = head tile
getEdge tile E = map last tile
getEdge tile S = last tile
getEdge tile W = map head tile

edges :: Set [Char]
edges = Set.fromList $ do
  tile <- toList input
  tr <- d8
  return (head (runD8 tr tile))

edgeID :: Map [Char] EdgeId
edgeID = EdgeId <$> Map.fromList (zip (toList edges) [0..])
edgeIDs :: Set EdgeId
edgeIDs = Set.fromList (toList edgeID)

edgeToTiles :: Map EdgeId [TileId]
edgeToTiles = Map.fromListWith (++) (tiles ++ [(e, []) | e <- toList edgeIDs])
  where
    tiles = [(e, [t]) | ((t, _, _), e) <- Map.toList tileEdge]

tileEdge :: Map (TileId, D8, Side) EdgeId
tileEdge = Map.fromList $ do
  (tid, tile) <- Map.toList input
  tr <- d8
  s <- [N, E, S, W]
  let tile' = runD8 tr tile
      edge = getEdge tile' s
  return ((tid, tr, s), edgeID Map.! edge)

neighborSets :: Map TileId [TileId]
neighborSets = Map.fromList $ do
  (tid, tile) <- Map.toList input
  let neighbors = do
        s <- [N, E, S, W]
        e <- [getEdge tile s, reverse (getEdge tile s)]
        let eid = edgeID Map.! e
        n <- edgeToTiles Map.! eid
        return n
  return (tid, neighbors)

allowedWE :: Set ((TileId,D8),(TileId,D8))
allowedWE = Set.fromList $ do
  tid <- Map.keys input
  o <- d8
  tid2 <- Map.keys input
  o2 <- d8
  guard $ Map.lookup (tid, o, E) tileEdge == Map.lookup (tid2, o2, W) tileEdge
  return ((tid, o), (tid2, o2))

allowedNS :: Set ((TileId,D8),(TileId,D8))
allowedNS = Set.fromList $ do
  tid <- Map.keys input
  o <- d8
  tid2 <- Map.keys input
  o2 <- d8
  guard $ Map.lookup (tid, o, S) tileEdge == Map.lookup (tid2, o2, N) tileEdge
  return ((tid, o), (tid2, o2))

options :: Set k -> Ersatz (Map k E.Bit)
options s = do
  xs <- sequence $ Map.fromSet (\_ -> E.exists :: Ersatz E.Bit) s
  E.assert (exactlyOne (toList xs))
  return xs

fromopts :: Map k Bool -> k
fromopts map = head [i | (i, True) <- Map.toList map]

part1 :: Map (Int,Int) (TileId, D8)
part1 = Map.intersectionWith (,) tiles rotation
  where
    gridW = 12
    gridH = 12
    keys = [(x, y) | x <- [0..gridW-1], y <- [0..gridW-1]]
    keysSet = Set.fromList keys

    we_pairs = [((x,y), (x+1,y)) | (x,y) <- keys, x + 1 < gridW]
    ns_pairs = [((x,y), (x,y+1)) | (x,y) <- keys, y + 1 < gridH]

    tiles :: Map (Int,Int) TileId
    tiles = fmap fromopts $ head $ solve1 $ do
      grid <- sequence $ Map.fromSet (\p -> options (Map.keysSet input)) keysSet
      let lookupGrid p i = grid Map.! p Map.! i
      traverse_ (E.assert . exactlyOne) (transpose (toList <$> toList grid))
      for_ (we_pairs ++ ns_pairs) $ \(p,p2) -> do
        for_ (Map.toList neighborSets) $ \(tid, ns) -> do
          E.assert $ lookupGrid p tid E.==>
            E.or [lookupGrid p2 tid2 | tid2 <- ns]
      return grid

    rotation :: Map (Int,Int) D8
    rotation = fmap fromopts $ head $ solve1 $ do
      grid <- sequence $ Map.fromSet (\p -> options (Set.fromList d8)) keysSet
      let lookupGrid p o = grid Map.! p Map.! o
      for_ we_pairs $ \(p,p2) -> do
        E.assert $ E.or [E.and [lookupGrid p o, lookupGrid p2 o2]
                        | ((tid, o), (tid2, o2)) <- toList allowedWE,
                          tid == tiles Map.! p,
                          tid2 == tiles Map.! p2]
      for_ ns_pairs $ \(p,p2) -> do
        E.assert $ E.or [E.and [lookupGrid p o, lookupGrid p2 o2]
                        | ((tid, o), (tid2, o2)) <- toList allowedNS,
                          tid == tiles Map.! p,
                          tid2 == tiles Map.! p2]
      return grid

solved_part1 :: Map (Int,Int) (TileId, D8)
solved_part1 = part1 -- first TileId <$> unsafePerformIO (read <$> readFile "output/20.txt")

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
  print part1
  print part2
