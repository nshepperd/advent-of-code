{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import           Data.Foldable
import           Data.Function.Memoize
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
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
import qualified Ersatz.Solver.Minisat as E
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

loeb :: Functor f => f (f r -> r) -> f r
loeb f = let r = fmap ($r) f in r

data Atom = Lit Text | Ref Int
  deriving (Show)
type Rule = [[Atom]]

manyspace :: Parser String
manyspace = many (char ' ')

p_rule :: Parser Rule
p_rule = some $ do
  let lit = Lit <$> (char '"' *> someText letter <* char '"' <* manyspace)
      ref = Ref <$> (p_nat <* manyspace)
  parts <- some (lit <|> ref)
  optional (text "| ")
  return parts

input :: (Map Int Rule, [Text])
input = unsafePerformIO (parse p <$> T.readFile "input/19.txt")
  where
    p = do rules <- p_rules
           txts <- p_text
           return (rules, txts)
    p_rules = fmap Map.fromList $ some $ do
      i <- p_nat
      text ": "
      rule <- p_rule <* spaces
      return (i, rule)
    p_text = some $ do
      txt <- someText letter
      spaces
      return txt

part1 = count True $ map run (snd input)
  where
    p :: Map Int (Parser ())
    p = loeb (sub <$> fst input)
    sub rule ps = let atom (Lit txt) = void (text txt)
                      atom (Ref n) = ps Map.! n
                      option as = sequence_ (map atom as)
                  in asum $ map (try . option) rule
    p0 = (p Map.! 0) <* eof
    run txt = parseBool p0 txt

try' :: Codensity Parser a -> Codensity Parser a
try' (Codensity m) = Codensity (\k -> try (m k))

part2 = count True $ map run (snd input)
  where
    p :: Map Int (Codensity Parser ())
    p = loeb (sub <$> rules)
    rules = (Map.fromList [(8, parse p_rule "42 | 42 8"),
                           (11, parse p_rule "42 31 | 42 11 31")]
              <> fst input)
    sub rule ps = let atom (Lit txt) = lift (void (text txt))
                      atom (Ref n) = ps Map.! n
                      option as = sequence_ (map atom as)
                  in asum $ map (try' . option) rule
    p0 = (p Map.! 0) <* lift eof
    run txt = parseBool (lowerCodensity p0) txt
