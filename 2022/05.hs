{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split (chunksOf)
import qualified Data.Text.IO as T
import           Data.Traversable
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

input,sample :: ([String], [(Int,Int,Int)])
(input,sample) = (unsafePerformIO (parse p <$> T.readFile "input/05.txt"),
                  unsafePerformIO (parse p <$> T.readFile "sample/05.txt"))
  where
    p = do
      diagram <- some $ try $ do
        some (oneOf " []" <|> letter) <* string "\n"
      some (char ' ' <|> digit) <* spaces

      let stacks = map (filter isAlpha) $ transpose $ map (concat . map (take 1 . drop 1) . chunksOf 4) diagram

      moves <- some $ do
        text "move "
        n <- p_int
        text " from "
        a <- p_int
        text " to "
        b <- p_int
        spaces
        return (n, a, b)
      return (stacks, moves)

part1 (stacks, moves) = map head $ foldl move stacks moves
  where
    move stacks (n, a, b)  = [
      case i of
        i | i == a -> drop n stacc
        i | i == b -> reverse (take n (stacks!!(a-1))) ++ stacc
        _ -> stacc
      | (i, stacc) <- zip [1..] stacks]

part2 (stacks, moves) = map head $ foldl move stacks moves
  where
    move stacks (n, a, b)  = [
      case i of
        i | i == a -> drop n stacc
        i | i == b -> take n (stacks!!(a-1)) ++ stacc
        _ -> stacc
      | (i, stacc) <- zip [1..] stacks]
