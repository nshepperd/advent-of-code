{-# Language ConstraintKinds #-}
{-# Language OverloadedStrings #-}

module Util where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Codensity
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Constraint
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Ersatz as E
import qualified Ersatz.Counting as E
import qualified Ersatz.Solver.Minisat as E
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

maximumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumOn f = maximumBy (comparing f)

minimumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
minimumOn f = minimumBy (comparing f)

count :: (Foldable f, Eq a) => a -> f a -> Int
count x xs = getSum $ foldMap (\y -> if y == x then Sum 1 else Sum 0) xs

the :: (Foldable f, Eq a) => f a -> a
the = theL . toList
  where
    theL [] = error "empty list"
    theL (x:xs) | all (==x) xs = x
    theL xs = error "not unique"

type Parser = Trifecta.Parser

parse :: Parser a -> Text -> a
parse p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show (Trifecta._errDoc e))

parseE :: Parser a -> Text -> Either String a
parseE p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> Right a
  Trifecta.Failure e -> Left (show (Trifecta._errDoc e))

parseString :: Parser a -> String -> a
parseString p txt = case Trifecta.parseString p mempty txt of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show (Trifecta._errDoc e))

-- Return True if the parser is a match for the full string.
parseBool :: Parser a -> Text -> Bool
parseBool p txt = case Trifecta.parseString (p <* eof) mempty (T.unpack txt) of
  Trifecta.Success a -> True
  Trifecta.Failure e -> False

p_nat :: (Read a, Integral a) => Parser a
p_nat = read <$> some digit

p_int :: (Read a, Integral a) => Parser a
p_int = (char '+' *> p_nat) <|> (char '-' *> fmap negate p_nat) <|> p_nat

someText :: Parser Char -> Parser Text
someText p = T.pack <$> some p

letters :: Parser Text
letters = T.pack <$> some letter

alnums :: Parser Text
alnums = T.pack <$> some alphaNum

(##) :: Monoid a => Parser a -> Parser a -> Parser a
(##) a b = (<>) <$> a <*> b

solve1 :: (E.Equatable a, E.Codec a) => Ersatz a -> [E.Decoded a]
solve1 m = case unsafePerformIO (E.solveWith E.anyminisat m) of
  (E.Satisfied, Just x) -> [x]
  _ -> []

solves :: (E.Equatable a, E.Codec a) => Ersatz a -> [E.Decoded a]
solves m = go []
  where
    go nots = case solve1 m' of
      [x] -> x : go (x : nots)
      _ -> []
      where
        m' = do a <- m
                for_ nots (\x -> E.assert (a E./== E.encode x))
                return a

type Ersatz = StateT E.SAT IO

-- exactly :: Int -> [E.Bit] -> E.Bit
-- exactly n xs = sum (map E.bits xs) E.=== fromIntegral n

exactlyOne :: [E.Bit] -> E.Bit
exactlyOne xs = E.exactly 1 xs
