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
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Numeric.Search.Range
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
-- import qualified Util.Text as T

parseInput :: Parser [(Text, Text)]
parseInput = some $ do
  a <- someText letter
  char '-'
  b <- someText letter
  spaces
  return (a,b)

input = unsafePerformIO (parse parseInput <$> T.readFile "input/12.txt")
sample = unsafePerformIO (parse parseInput <$> T.readFile "input/12_sample.txt")

part1 input = dfs Set.empty "start" ["start"]
  where
    dfs visited node path
      | node == "end" = [path]
      | T.all isLower node && Set.member node visited = []
      | otherwise = fold [dfs visited' e (e:path) | e <- edges Map.! node]
      where
        visited' = Set.insert node visited
    nodes = Set.fromList [x | (a,b) <- input, x <- [a,b]]
    edges = Map.fromListWith (<>) [(x, [y]) | (a,b) <- input, (x,y) <- [(a,b),(b,a)]]

part2 input = dfs Set.empty False "start" ["start"]
  where
    dfs visited vsmall node path
      | node == "end" = [path]
      | T.all isLower node && Set.member node visited && vsmall = []
      | T.all isLower node && Set.member node visited && node == "start" = []
      | T.all isLower node && Set.member node visited = fold [dfs visited' True e (e:path) | e <- edges Map.! node]
      | otherwise = fold [dfs visited' vsmall e (e:path) | e <- edges Map.! node]
      where
        visited' = Set.insert node visited
    nodes = Set.fromList [x | (a,b) <- input, x <- [a,b]]
    edges = Map.fromListWith (<>) [(x, [y]) | (a,b) <- input, (x,y) <- [(a,b),(b,a)]]
