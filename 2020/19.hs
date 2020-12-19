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
import qualified Text.ParserCombinators.ReadP as ReadP

import           Util
import qualified Util.Text as T

loeb :: Functor f => f (f r -> r) -> f r
loeb f = let r = fmap ($r) f in r

input :: (Map Int Text, [Text])
input = unsafePerformIO (parse p <$> T.readFile "input/19.txt")
  where
    p = do rules <- p_rules
           txts <- p_text
           return (rules, txts)
    p_rules = fmap Map.fromList $ some $ do
      a <- p_nat
      text ": "
      b <- someText (noneOf "\n")
      spaces
      return (a, b)
    p_text = some $ do
      txt <- someText letter
      spaces
      return txt

part1 = map (parse ptry) (snd input)
  where
    p :: Map Int (Parser Text)
    p = loeb (sub <$> fst input)
    raw = char '"' *> (text <$> someText (noneOf "\"")) <* char '"' <* spaces
    sub txt rules = let ref = (rules Map.!) <$> p_nat <* spaces
                        p_rule = do
                          options <- some $ do
                            parts <- some (raw <|> ref)
                            optional (char '|')
                            spaces
                            return (foldr1 (##) parts)
                          return (asum (map try options))
                    in parse (p_rule <* eof) txt
    p0 = (p Map.! 0) <* eof
    ptry = try (const True <$> p0) <|> pure False

part2 = count True $ map run (snd input)
  where
    rules = (Map.fromList [(8, "42 | 42 8"),
                           (11, "42 31 | 42 11 31")] <> fst input)
    p :: Map Int (ReadP.ReadP ())
    p = loeb (sub <$> rules)
    raw = do char '"'
             c <- (noneOf "\"")
             char '"'
             return (const () <$> ReadP.char c)
    sub txt rules = let ref = (rules Map.!) <$> p_nat <* spaces
                        p_rule = do
                          options <- some $ do
                            parts <- some (raw <|> ref)
                            optional (char '|')
                            spaces
                            return (foldr1 (*>) parts)
                          return (asum options)
                    in parse (p_rule <* eof) txt
    p0 = (p Map.! 0) <* eof
    run txt = not $ null $ ReadP.readP_to_S p0 (T.unpack txt)
