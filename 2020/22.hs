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
import           Data.Sequence (Seq, ViewR(..), ViewL(..), (<|), (|>))
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
import qualified Ersatz.Solver.Minisat as E
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

input :: [[Int]]
input = unsafePerformIO (parse p <$> T.readFile "input/22.txt")
  where
    p = some $ do
      text "Player "
      p_nat
      text ":"
      spaces
      some (p_nat <* spaces)

type Deck = Seq Int
type Player = Int
data Outcome = Outcome Player Deck
  deriving Show

combat :: [Deck] -> Outcome
combat decks = case Seq.viewl <$> decks of
  [a :< as, b :< bs]
    | a > b -> combat [as |> a |> b, bs]
    | b > a -> combat [as, bs |> b |> a]
  [a :< as, EmptyL] -> Outcome 0 (a <| as)
  [EmptyL, b :< bs] -> Outcome 1 (b <| bs)

score :: Deck -> Int
score deck = sum $ zipWith (*) (reverse (toList deck)) [1..]

part1 :: Int
part1 = case combat (map Seq.fromList input) of
  Outcome p xs -> score xs

recursive :: Set [Deck] -> [Deck] -> Outcome
recursive visited decks
  | Set.member decks visited = Outcome 0 (decks !! 0)
  | otherwise = case Seq.viewl <$> decks of
      [a :< as, b :< bs]
        | a <= length as && b <= length bs ->
          case recursive Set.empty [Seq.take a as, Seq.take b bs] of
            Outcome 0 _ -> recursive visited' [as |> a |> b, bs]
            Outcome 1 _ -> recursive visited' [as, bs |> b |> a]
        | a > b -> recursive visited' [as |> a |> b, bs]
        | b > a -> recursive visited' [as, bs |> b |> a]
      [a :< as, EmptyL] -> Outcome 0 (a <| as)
      [EmptyL, b :< bs] -> Outcome 1 (b <| bs)
      [EmptyL, EmptyL] -> error "empty"
  where
    visited' = Set.insert decks visited

part2 :: Int
part2 = case recursive Set.empty (map Seq.fromList input) of
  Outcome 0 xs -> score xs
