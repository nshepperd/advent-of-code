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
import           Linear.Vector
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

type Item = Map Char Int

input, sample :: [(Char, Text, [Text])]
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/20.txt", "sample/20.txt"]
  where
    p = some $ do
      ty <- oneOf "%&" <|> pure '='
      label <- letters
      " -> "
      conn <- (some $ letters <* optional ", ") <* " "
      return (ty, label, conn)

data Pulse = High | Low
  deriving (Show,Eq,Ord)

data Mod = Broadcast | FlipFlop Int | Conj (Map Text Pulse)
  deriving (Show,Eq,Ord)

modBroadcast = Broadcast
modFlipFlop = FlipFlop 0
modConj inputs = Conj (Map.fromList [(lab,Low) | lab <- inputs])

runMod :: Mod -> Text -> Pulse -> (Maybe Pulse, Mod)
runMod Broadcast lab p = (Just p, Broadcast)
runMod (FlipFlop s) lab High = (Nothing, FlipFlop s)
runMod (FlipFlop 1) lab Low = (Just Low, FlipFlop 0)
runMod (FlipFlop 0) lab Low = (Just High, FlipFlop 1)

runMod (Conj mem) lab p
  | all (==High) mem' = (Just Low, Conj mem')
  | otherwise         = (Just High, Conj mem')
  where
    mem' = Map.insert lab p mem

-- data Mod = Mod { runMod :: Text -> Pulse -> (Maybe Pulse, Mod)}

-- modBroadcast = Mod (\lab p -> (Just p, modBroadcast))
-- modFlipFlop = Mod (go 0)
--   where
--     go s lab High = (Nothing, Mod (go s))
--     go 1 lab Low = (Just Low, Mod (go 0))
--     go 0 lab Low = (Just High, Mod (go 1))

-- modConj inputs = Mod (go (Map.fromList [(lab,Low) | lab <- inputs]))
--   where
--     go mem lab pulse = check (Map.insert lab pulse mem)
--     check mem
--       | all (==High) mem = (Just Low, Mod (go mem))
--       | otherwise = (Just High, Mod (go mem))

dot input = "digraph { " <> T.unlines [label <> " -> " <> out  | (ty, label, conn) <- input, out <- conn] <> "}"

part1 input = finish $ send modules [] (replicate 1000 ("button", "broadcaster", Low))
  where
    finish pulses = product count
      where
        count = Map.fromListWith (+) [(p,1) | p <- pulses]
    inputs = Map.fromListWith (++) [(out, [label]) | (ty,label,conn) <- input, out <- conn]
    outputs = Map.fromList [(label, conn) | (ty,label,conn) <- input]
    modules = Map.fromList [(label, case ty of
                                '=' -> modBroadcast
                                '%' -> modFlipFlop
                                '&' -> modConj (inputs Map.! label))
                              | (ty,label,conn) <- input]

    send modules [] [] = []
    send modules [] (u:us) = send modules [u] us
    send modules ((from,to,pulse):pulses) us
      | Map.member to modules = case runMod (modules Map.! to) from pulse of
          (Just p, newmod) -> pulse : send (Map.insert to newmod modules) (pulses ++ [(to, o, p) | o <- outputs Map.! to]) us
          (Nothing, newmod) -> pulse : send (Map.insert to newmod modules) pulses us
      | otherwise = pulse : send modules pulses us

type Signal = (Text,Text,Pulse)
type Modules = Map Text Mod

part2 input = foldr lcm 1 [len-1 | (n,len,loopi) <- map findcycle parts_in]
  where
    findcycle part = (head [i | (i,True) <- zip [1..] loop], length loop, loopi)
      where
        (loopi, loop) = go modules Map.empty 0 []
        go modules visited i gg
          | Map.member modules visited = (visited Map.! modules, gg)
          | otherwise = case send modules [] [("broadcaster", part, Low)] of
                          (newmod, sent) -> let isgood = length [(from, "dn") | (from, "dn", High) <- sent] > 0
                                            in go newmod (Map.insert modules i visited) (i+1) (gg ++ [isgood])

    parts_in = outputs Map.! "broadcaster"
    inputs = Map.fromListWith (++) [(out, [label]) | (ty,label,conn) <- input, out <- conn]
    outputs = Map.fromList [(label, conn) | (ty,label,conn) <- input]
    modules = Map.fromList [(label, case ty of
                                '=' -> modBroadcast
                                '%' -> modFlipFlop
                                '&' -> modConj (inputs Map.! label))
                              | (ty,label,conn) <- input]

    send :: Modules -> [Signal] -> [Signal] -> (Modules, [Signal])
    send modules sent [] = (modules, sent)
    send modules sent ((from,to,pulse):pulses)
      | Map.member to modules = case runMod (modules Map.! to) from pulse of
          (Just p, newmod) -> send (Map.insert to newmod modules) sent' (pulses ++ [(to, o, p) | o <- outputs Map.! to])
          (Nothing, newmod) -> send (Map.insert to newmod modules) sent' pulses
      | otherwise = send modules sent' pulses
      where
        sent' = ((from,to,pulse):sent)
