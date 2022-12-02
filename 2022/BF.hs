{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BF where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

data Machine r = In (Int -> Machine r)
               | Out Int (Machine r)
               | Stop r
               deriving Functor

match :: String -> [Int]
match program = [mat i | i <- [0..length program - 1]]
  where
    _levels = levels program
    ilevels = zip [0..] (levels program)
    mat i = case program!!i of
              '[' -> head [j | (j, l) <- drop (i+1) ilevels, l == (_levels !! i)] - 1
              ']' -> last [j | (j, l) <- take i ilevels, l == (_levels !! i)-1]
              _ -> 0
levels program = let go l '[' = l + 1
                     go l ']' = l - 1
                     go l _ = l
                 in scanl go 0 program


bf :: String -> Machine (Map Int Int)
bf program = go 0 0 Map.empty
  where
    matching = match program
    go pc p mem | pc >= length program = Stop mem
    go pc p mem = case program!!pc of
      '>' -> go (pc+1) (p + 1) mem
      '<' -> go (pc+1) (p - 1) mem
      '+' -> go (pc+1) p (Map.insert p (look p + 1) mem)
      '-' -> go (pc+1) p (Map.insert p (look p - 1) mem)
      '.' -> Out (look p) (go (pc+1) p mem)
      ',' -> In (\v -> go (pc+1) p (Map.insert p v mem))
      '[' -> case look p of
               0 -> go ((matching!!pc)+1) p mem
               _ -> go (pc+1) p mem
      ']' -> case look p of
               0 -> go (pc+1) p mem
               _ -> go ((matching!!pc)+1) p mem
      ' ' -> go (pc+1) p mem
      '\n' -> go (pc+1) p mem
      '\t' -> go (pc+1) p mem
      c -> error (show c)
      where
        look p = Map.findWithDefault 0 p mem

bf' :: String -> [Int] -> [Int]
bf' program inp = go (bf program) inp
  where
    go (In f) (x:xs) = go (f x) xs
    go (Out x next) xs = x:(go next xs)
    go (Stop r) xs = []
