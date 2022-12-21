{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Numeric.Search.Range
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Lens
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
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
import qualified Ersatz.Counting as E
import qualified Ersatz.Solver.Minisat as E
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util

data Expr = EInt Int | EOp Text Char Text
  deriving Show

input, sample :: Map Text Expr
[input, sample] = fmap (Map.fromList . parse p . unsafePerformIO . T.readFile) ["input/21.txt", "sample/21.txt"]
  where
    p = some $ do
      name <- someText letter
      char ':'
      spaces
      let o_int = EInt <$> p_int <* spaces
          o_op = do
            a <- someText letter <* spaces
            o <- oneOf "*+-/" <* spaces
            b <- someText letter <* spaces
            return (EOp a o b)
      op <- o_int <|> o_op
      return (name, op)

part1 input = res Map.! "root"
  where
    res = f <$> input
    f (EInt n) = fromIntegral n :: Rational
    f (EOp a o b) = op v_a v_b
      where
        op = case o of
          '*' -> (*)
          '+' -> (+)
          '-' -> (-)
          '/' -> (/)
        v_a = res Map.! a
        v_b = res Map.! b

part2 input = searchFromTo (\n -> func (fromIntegral n) /= func 0) 0 6000000000000
  where
    func = (res Map.! "root")
    res = Map.mapWithKey f input
    f "humn" (EInt n) = id
    f      _ (EInt n) = const (fromIntegral n :: Rational)
    f name (EOp a o b) = op <$> v_a <*> v_b
      where
        op = case name of
          "root" -> \a b -> fromIntegral $ fromEnum (compare a b) - 1
          _ -> case o of
            '*' -> (*)
            '+' -> (+)
            '-' -> (-)
            '/' -> (/)
        v_a = res Map.! a
        v_b = res Map.! b
