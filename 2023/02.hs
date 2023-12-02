{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

import           Util

-- data Color = R | G | B
--   deriving (Show,Eq,Ord)

input, sample :: [(Int, [[(Int,Text)]])]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/02.txt", "sample/02.txt"]
  where
    p = some $ do
      text "Game "
      gid <- p_nat
      text ": "
      record <- flip sepBy1 (text "; ") $ do
        flip sepBy1 (text ", ") $ do
          n <- p_nat
          spaces
          color <- text "red" <|> text "green" <|> text "blue"
          return (n, color)
      spaces
      return (gid, record)


part1 :: [(Int, [[(Int,Text)]])] -> Int
part1 input = sum (map fst (filter (check.snd) input))
  where
    check game = all (all checkb) game
    checkb (n,"red") = n <= 12
    checkb (n,"green") = n <= 13
    checkb (n,"blue") = n <= 14

part2 :: [(Int, [[(Int,Text)]])] -> Int
part2 input = sum (map (product . check . snd) input)
  where
    check game = map collect ["red", "green", "blue"]
      where
        collect color = maximum (map (sum . map (checkc color)) game)
        checkc c (n,nc) | c == nc = n
                        | c /= nc = 0
