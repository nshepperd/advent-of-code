{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import           Data.Decimal
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ratio
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

input :: [Int]
input = map (read . pure) "193467258"

move :: [Int] -> [Int]
move (c:cs) = take (dest+1) remaining ++ picked_up ++ drop (dest+1) remaining ++ [c]
  where
    picked_up = take 3 cs
    remaining = drop 3 cs
    dest = head [i | a <- [c-1,c-2..1] ++ [9,8..1], Just i <- [findIndex (==a) remaining]]

part1 :: String
part1 = foldMap show (drop (i+1) final ++ take i final)
  where
    final = iterate move input !! 100
    Just i = findIndex (==1) final

-- Part 2

type Label = Int
type Prio = [Int]

type Deck = (Map Prio Label, Map Label Prio)

base i = [i]

between3 :: Prio -> Prio -> [Prio]
between3 [] (b:bs) = between3 [0] (b:bs)
between3 (a:as) (b:bs)
  | a + 3 < b = [[a+1], [a+2], [a+3]]
  | a     < b = (a:) <$> after3 as
  | a    == b = (a:) <$> between3 as bs

after3 :: Prio -> [Prio]
after3 [] = [[1], [2], [3]]
after3 (a:as) = [[a+1], [a+2], [a+3]]

after1 :: Prio -> Prio
after1 [] = [1]
after1 (a:as) = [a+1]

move2 :: Deck -> Deck
move2 (!p2l, !l2p) = (p2l_3, l2p_3)
  where
    max_label = fst (Map.findMax l2p)

    (cp, cl) = Map.findMin p2l
    picked_up = drop 1 $ take 4 $ Map.toAscList p2l
    picked_up_ls = map snd picked_up

    p2l_1 = Map.drop 4 p2l
    l2p_1 = foldr Map.delete l2p (cl : picked_up_ls)

    isAvailable :: Label -> Bool
    isAvailable x = not (elem x (cl : picked_up_ls))
    dest_al = head [a | a <- [cl - 1, cl - 2..1] ++ [max_label,max_label-1..1], isAvailable a]
    dest_ap = l2p_1 Map.! dest_al
    dest_ps = case Map.lookupGT dest_ap p2l_1 of
                Just (dest_bp,_) -> between3 dest_ap dest_bp
                Nothing -> after3 dest_ap

    p2l_2 = Map.fromList (zip dest_ps picked_up_ls) <> p2l_1
    l2p_2 = Map.fromList (zip picked_up_ls dest_ps) <> l2p_1

    last_prio = after1 (fst (Map.findMax p2l_2))
    p2l_3 = Map.insert last_prio cl p2l_2
    l2p_3 = Map.insert cl last_prio l2p_2

run2 :: [Int] -> Int -> [Int]
run2 input i = case Map.split (l2p Map.! 1) p2l of
                 (l, r) -> take 10 (Map.elems r <> Map.elems l)
  where
    start = (Map.fromList (zip (base <$> [1..]) input),
             Map.fromList (zip input (base <$> [1..])))
    final = iterate move2 start !! i
    (p2l, l2p) = final

part2input :: [Int]
part2input = input ++ [maximum input + 1 .. 1000000]

part2 :: Int
part2 = case take 2 (run2 part2input 10000000) of
  [a, b] -> a * b

main = print part2
