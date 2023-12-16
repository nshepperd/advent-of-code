{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Linear.V2
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators (eof)
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: [String]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/15.txt", "sample/15.txt"]
  where
    p = some $ some (letter <|> digit <|> oneOf "-=") <* optional "," <* spaces

hash :: String -> Int
hash str = fromIntegral $ sum $ zipWith (*) [fromIntegral (ord c) :: Word8 | c <- reverse str] (iterate (*17) 17)

part1 :: [String] -> Int
part1 input = sum $ map hash input

atL :: Eq a => a -> Lens' [(a,b)] (Maybe b)
atL a = lens get set
  where
    get xs = lookup a xs
    set xs (Just b) = case get xs of
      Just _ -> [(k, if k == a then b else v) | (k,v) <- xs]
      Nothing -> xs ++ [(a,b)]
    set xs Nothing = [(k,v) | (k,v) <- xs, k /= a]

part2 :: [String] -> Int
part2 input = foldr go finish input (replicate 256 [])
  where
    finish boxes = sum [i*j*n | (i,box) <- zip [1..] boxes, (j,(label,n)) <- zip [1..] box]
    go step k boxes = k $ case ins of
      ('=':ns) -> boxes & ix i . atL label .~ Just (read ns :: Int)
      ['-'] -> boxes & ix i . atL label .~ Nothing
      where
        (label, ins) = span isAlpha step
        i = hash label
