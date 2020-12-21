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

input :: [([Text], [Text])]
input = unsafePerformIO (parse p <$> T.readFile "input/21.txt")
  where
    p = some $ do
      ingredients <- some (someText letter <* spaces)
      char '('
      text "contains" <* spaces
      allergens <- some (someText letter <* optional (char ',') <* spaces)
      char ')'
      spaces
      return (ingredients, allergens)

options' s = do
  xs <- sequence $ Map.fromSet (\_ -> E.exists :: Ersatz E.Bit) s
  E.assert (exactlyOne (toList xs))
  return xs

fromopts :: Map k Bool -> [k]
fromopts map = [i | (i, True) <- Map.toList map]

iset = Set.fromList (foldMap fst input)
aset = Set.fromList (foldMap snd input)
free = iset `Set.difference` (Set.fromList (fold solution))
solution = fromopts <$> head (solve1 puzzle)
puzzle = do
  atoi <- sequence $ Map.fromSet (\a -> options' iset) aset
  for_ iset $ \i -> do
    E.assert $ E.atmost 1 [atoi Map.! a Map.! i | a <- toList aset]
  for_ input $ \(is,as) -> do
    for_ as $ \a -> do
      E.assert $ E.or [atoi Map.! a Map.! i | i <- is]
  return atoi

part1 :: Int
part1 = length [i | (is, as) <- input, i <- is, Set.member i free]

part2 :: Text
part2 = T.intercalate "," (fold solution)
