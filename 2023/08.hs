{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Control.Lens
import           Data.Char
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Unsafe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta
import           Debug.Trace
import           Numeric.Search.Range
import qualified Data.Vector as V

import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

input, sample :: (String,Map Text (Text,Text))
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/08.txt", "sample/08.txt"]
  where
    p = do
      steps <- some (oneOf "LR") <* spaces
      nodes <- some $ do
        a <- letters <* " = ("
        b <- letters <* " , "
        c <- letters <* " ) "
        return (a, (b,c))
      return (steps, Map.fromList nodes)

type Hand = String

part1 (steps, graph) = go "AAA" 0 (cycle steps)
  where
    go node n (s:steps)
      | node == "ZZZ" = n
      | s == 'L' = go l (n+1) steps
      | s == 'R' = go r (n+1) steps
      where
        (l, r) = graph Map.! node

getindex xs a = length (takeWhile (/= a) xs)
assert True x = x

part2 (steps, graph) = foldr1 lcm $ map findz [n | n <- Map.keys graph, T.last n == 'A']
  where
    -- The input seems pretty convenient. There's two things that we rely on that would have been false with a different input
    -- - [zi]. We only visit **Z once per loop, so we don't need to consider multiple possible ending points.
    -- - zi == period. The time until we first visit **Z is the same as the period of the loop, so we can just use the lcm of the periods.
    findz start = assert (zi == period) zi
      where
        path = mkpath start 0 Set.empty
        loopnode = last path
        loopstart = getindex path loopnode
        period = length path - loopstart - 1
        -- prologue = fst <$> take loopstart path
        loop = fst <$> (take period . drop loopstart) path
        [zi] = [i | (i,n) <- zip [loopstart..] loop, T.last n == 'Z']

    vsteps = V.fromList steps
    mkpath node si visited
      | Set.member (node,si) visited = [(node,si)]
      | otherwise = (node,si) : mkpath next (mod (si + 1) (V.length vsteps)) (Set.insert (node,si) visited)
      where next = case vsteps V.! si of
              'L' -> fst $ graph Map.! node
              'R' -> snd $ graph Map.! node
