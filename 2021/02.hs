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
-- import qualified Util.Text as T

input :: [(Text, Int)]
input = unsafePerformIO (parse p <$> T.readFile "input/02.txt")
  where
    p = some $ do
      dir <- someText letter <* spaces
      l <- p_int <* spaces
      return (dir, l)

step ("down", n) = V2 0 n
step ("up", n) = V2 0 (-n)
step ("forward", n) = V2 n 0

part1 = product (sum (map step input))

step2 ("down", n) (x,y,a) = (x,y,a+n)
step2 ("up", n) (x,y,a) = (x,y,a-n)
step2 ("forward", n) (x,y,a) = (x+n,y+a*n,a)

part2 = case foldl (flip step2) (0,0,0) input of
  (x,y,a) -> x*y
