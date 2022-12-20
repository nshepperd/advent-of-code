{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

input, sample :: [Int]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/20.txt", "sample/20.txt"]
  where
    p = some (p_int <* spaces)

move :: (Integral a) => (Int,a) -> Seq (Int,a) -> Seq (Int,a)
move (j,n) seq = case Seq.splitAt i seq of
                   (l, _ Seq.:<| r) -> Seq.insertAt (fromIntegral $ n `mod` size) (j,n) (r <> l)
  where
    Just i = Seq.elemIndexL (j,n) seq
    size = fromIntegral (Seq.length seq - 1)

part1 input = sum [snd $ Seq.index finalseq i | i <- map (`mod` (Seq.length finalseq)) [i0+1000,i0+2000,i0+3000]]
  where
    numbers = [(j, n) | (j,n) <- zip [1..] input]
    Just i0 = Seq.elemIndexL 0 (snd <$> finalseq)
    finalseq = mix (Seq.fromList numbers)
    mix seq = foldr go finish numbers seq
    go n k !seq = k (move n seq)
    finish seq = seq

part2 input = sum [snd $ Seq.index finalseq i | i <- map (`mod` (Seq.length finalseq)) [i0+1000,i0+2000,i0+3000]]
  where
    numbers = [(j, 811589153*n) | (j,n) <- zip [1..] input]
    Just i0 = Seq.elemIndexL 0 (snd <$> finalseq)
    finalseq = iterate mix (Seq.fromList numbers) !! 10
    mix seq = foldr go finish numbers seq
    go n k !seq = k (move n seq)
    finish seq = seq
