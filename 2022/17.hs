{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Debug.Trace
import           Linear.V2
import           System.IO
import           System.IO.Unsafe

import           Util

sample = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" :: String

paragraphs :: Text -> [[Text]]
paragraphs txt = map T.lines (T.splitOn "\n\n" txt)

data Rocc = Rocc { roccWidth :: Int, roccHeight :: Int, roccSet :: Set (V2 Int) }
  deriving Show

roccs :: [Rocc]
roccs = map p . paragraphs . unsafePerformIO $ T.readFile "input/17roccs.txt"
  where
    p lines = Rocc {
      roccWidth = T.length (head lines),
      roccHeight = length lines,
      roccSet = Set.fromList [V2 x y | (y, line) <- zip [0..] (reverse lines), (x, '#') <- zip [0..] (T.unpack line)]
      }

input :: String
[input] = fmap (filter (`elem` ['<','>']) . unsafePerformIO . readFile) ["input/17.txt"]

draw tower = putStrLn $ unlines $ take 20 [drawLine y | y <- reverse [-1..height]]
  where
    height = maximum (map (view _y) (toList tower)) + 1
    drawLine (-1) = "+" ++ replicate 7 '-' ++ "+"
    drawLine y = "|" ++ [if Set.member (V2 x y) tower then '#' else '.' | x <- [0..6]] ++ "|"

part1 input = loop !! 2021
  where
    loop = go Set.empty 0 (cycle roccs) (cycle input)
    go tower height roccList jetList = push (head roccList) (V2 2 (height + 3)) (jetList)
      where
        inwall (V2 x y) = x < 0 || x >= 7 || y < 0
        collides rocc pos = any (\p -> Set.member p tower || inwall p) [p + pos | p <- toList (roccSet rocc)]
        push rocc pos (j:jetList)
          | collides rocc (pos + d) = fall rocc pos jetList
          | otherwise = fall rocc (pos + d) jetList
          where d = case j of
                  '<' -> V2 (-1) 0
                  '>' -> V2 1 0
        fall rocc pos jetList
          | collides rocc (pos - V2 0 1) = let tower' = tower <> Set.map (+pos) (roccSet rocc)
                                               height' = max height (roccHeight rocc + view _y pos)
                                               roccList' = tail roccList
                                           in height' : go tower' height' roccList' jetList
          | otherwise = push rocc (pos - V2 0 1) jetList

part2 input = case findFirstLoop of
                (r,k) -> let clen = k - r
                             cycles = (1000000000000 - r) `div` clen
                             remainder = (1000000000000 - r) `mod` clen
                         in getHeightK (r + remainder) + cycles * (getHeightK k - getHeightK r)
  where
    getHeightK k = getHeight (loop !! k)
    getHeight (i,j,height,r,k) = height
    getIJ (i,j,height,r,k) = (i,j)
    findFirstLoop = head [(r,k) | (i,j,height,Just r,k) <- loop,
                          map getIJ (take (k - r) $ drop r loop) == map getIJ (take (k - r) $ drop k loop)]

    loop = go Set.empty 0 0 0 Map.empty 0
    go tower height i j seen k = (i, j, height, Map.lookup (i,j) seen, k) : push (roccs !! i) (V2 2 (height + 3)) j
      where
        seen' = Map.insert (i,j) k seen
        inwall (V2 x y) = x < 0 || x >= 7 || y < 0
        collides rocc pos = any (\p -> Set.member p tower || inwall p) [p + pos | p <- toList (roccSet rocc)]
        push rocc pos j
          | collides rocc (pos + d) = fall rocc pos ((j+1) `mod` length input)
          | otherwise = fall rocc (pos + d) ((j+1) `mod` length input)
          where d = case input !! j of
                  '<' -> V2 (-1) 0
                  '>' -> V2 1 0
        fall rocc pos j
          | collides rocc (pos - V2 0 1) = let tower' = tower <> Set.map (+pos) (roccSet rocc)
                                               height' = max height (roccHeight rocc + view _y pos)
                                               i' = (i + 1) `mod` length roccs
                                           in go tower' height' i' j seen' (k+1)
          | otherwise = push rocc (pos - V2 0 1) j
