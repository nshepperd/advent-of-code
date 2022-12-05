{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

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

import           BF
import           Util

here :: String -> String
here tag = unsafePerformIO $ do
  txt <- readFile __FILE__
  let (_,_:rest) = span (/="{- "++tag++" START") (lines txt)
      (doc,_) = span (/="   "++tag++" END -}") rest
  return (unlines doc)

type Range = (Int,Int)

input,sample :: [(Range,Range)]
(input,sample) = (unsafePerformIO (parse p <$> T.readFile "input/04.txt"),
                  unsafePerformIO (parse p <$> T.readFile "sample/04.txt"))
  where
    p_range = do
      l <- p_int
      char '-'
      r <- p_int
      return (l,r)
    p = some $ do
      a <- p_range
      char ','
      b <- p_range
      spaces
      return (a,b)

part1 input = bf' (here "part1") (concat [[a,b,c,d] | ((a,b),(c,d)) <- input] ++ [0])

{- part1 START

,[>,>,>,<<<

# !a | b | c | d | _

[->>>>> +>>>>>+<<<<< <<<<<]>
[->>>> >>>+>>>+ <<< <<< <<<<]>
[->>> >+>>>+ <<<< <<<]>
[->> >>+>>>>>+ << <<<<< <<]>

>

# _ | !a | c | d | b | c | a | b | d

[
+>[-<->]<  [[-]>+<]
>>
]
<<<<<<<<


# !0 | a≥c | 0 | d≥b | 0 | c≥a | 0 | b≥d

>[>>[<<<+>>>[-]]<<[-]] >>[-]<< <

# !a≥c&b≤d | 0 | 0 | 0 | 0 | c≥a | 0 | b≥d

>>>> >[>>[<<<+>>>[-]]<<[-]] >>[-]<< <

# a≥c&b≤d | 0 | 0 | 0 | !c≥a & b≤d | 0 | 0 | 0

[-<<<<+>>>>]<<<<

# _ | !(a≥c & b≤d)or(c≥a & b≤d) | 0 | 0 | 0 | 0 | 0 | 0 | 0

[[-]<+>]

<.>

<<<<<

,]

   part1 END -}

inter :: Range -> Range -> Bool
inter (a,b) (c,d) = max a c <= min b d

sub :: Range -> Range -> Bool
sub (a,b) (c,d) = a >= c && b <= d
