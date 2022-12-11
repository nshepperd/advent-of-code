{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language GADTs #-}

import           Control.Applicative
import           Control.Lens
import           Data.List
import qualified Data.Text.IO as T
import           Debug.Trace
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

data Unit = Old | N Int
  deriving Show

eval :: [[Unit]] -> Int -> Int
eval terms x = sum [evalp t x | t <- terms]
  where
    evalp factors x = product [evalu f x | f <- factors]
    evalu Old x = x
    evalu (N n) _ = n

data Monkey = Monkey {
  startingItems :: [Int],
  operation :: [[Unit]],
  test :: Int,
  targets :: (Int,Int)
  }
  deriving Show

-- input, sample :: _
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/11.txt", "sample/11.txt"]
  where
    p_unit = ((Old <$ text "old") <|> (N <$> p_nat)) <* spaces
    p_factor = p_unit `sepBy1` (char '*' <* spaces)
    p_term = p_factor `sepBy1` (char '+' <* spaces)
    p_op = p_term
    p = some $ do
      text "Monkey " >> p_nat >> text ":" >> spaces
      items <- text "Starting items: " *> some (p_nat <* optional (char ',') <* spaces)
      op <- text "Operation: new = " *> p_op <* spaces
      test <- text "Test: divisible by " *> p_nat <* spaces
      next <- do
        t <- text "If true: throw to monkey " *> p_nat <* spaces
        f <- text "If false: throw to monkey " *> p_nat <* spaces
        return (t,f)
      return (Monkey items op test next)

part1 input = product $ take 2 $ reverse $ sort $ map sum $ transpose $ take 20 $ go (map startingItems input)
  where
    n = length input
    go items = foldr step finish [0..n-1] items
    finish items = [] : go items
    step i k items = let items' = foldr f z (items!!i) (items & ix i .~ [])
                         (x:xs) = k items'
                     in (length (items!!i) : x) : xs
      where
        op = operation (input!!i)
        testval = test (input!!i)
        target = targets (input!!i)
        f w k items = let w' = eval op w `div` 3
                          items' = case mod w' testval of
                            0 -> over (ix (fst target)) (++[w']) items
                            _ -> over (ix (snd target)) (++[w']) items
                      in k items'
        z items = items

part2 input = product $ take 2 $ reverse $ sort $ map sum $ transpose $ take 10000 $ go (map startingItems input)
  where
    mx = product (map test input)
    n = length input
    go items = foldr step finish [0..n-1] items []
    finish items inspects = reverse inspects : go items
    step i k items inspects = let items' = foldr f z (items!!i) (items & ix i .~ [])
                              in k items' (length (items!!i) : inspects)
      where
        op = operation (input!!i)
        testval = test (input!!i)
        target = targets (input!!i)
        f w k items = let w' = eval op w `mod` mx
                          items' = case mod w' testval of
                            0 -> over (ix (fst target)) (++[w']) items
                            _ -> over (ix (snd target)) (++[w']) items
                      in k items'
        z items = items
