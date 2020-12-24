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
import           Control.Monad.Codensity
import           Control.Monad.Trans
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
import           Linear.V3
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [[Text]]
input = unsafePerformIO (parse p <$> T.readFile "input/24.txt")
  where
    p = some $ do
      some (asum $ map text (T.words "e se sw w nw ne")) <* spaces

type Hex = V3 Int

move :: Text -> Hex
move "e" = V3 2 (-1) (-1)
move "nw" = V3 (-1) 1 0
move "sw" = V3 (-1) 0 1
move "w" = -(move "e")
move "se" = -(move "nw")
move "ne" = -(move "sw")

moves :: [Text] -> Hex
moves xs = sum (map move xs)

neighbors :: Hex -> [Hex]
neighbors hex = [hex + m | m <- map move (T.words "e se sw w nw ne")]

life :: Set Hex -> Set Hex
life tiles = Set.filter go border
  where
    countNeighbors x = length (filter (`Set.member` tiles) (neighbors x))
    go x = case (Set.member x tiles, countNeighbors x) of
      (True, 0) -> False
      (True, n) | n > 2 -> False
      (True, n) -> True
      (False, 2) -> True
      (False, _) -> False

    border = Set.fromList (foldMap neighbors tiles) <> tiles

part1 :: Int
part1 = length $ Map.filter odd $ Map.fromListWith (+) [(moves m, 1) | m <- input]

part2 :: Int
part2 = Set.size (iterate life start !! 100)
  where
    start = Map.keysSet $ Map.filter odd $ Map.fromListWith (+) [(moves m, 1) | m <- input]
