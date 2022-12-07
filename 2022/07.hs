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
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe
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

data LS = LS [Text] [(Text, Maybe Int)]
  deriving Show

input :: [LS]
(input,sample) = (unsafePerformIO (parse p <$> T.readFile "input/07.txt"),
                  unsafePerformIO (parse p <$> T.readFile "sample/07.txt"))
  where
    p = p_go [] []
    p_go dirs cwd = p_cd dirs cwd <|> p_ls dirs cwd <|> p_end dirs cwd
    p_cd dirs cwd = do
      text "$ cd"
      spaces
      name <- text "/" <|> text ".." <|> someText letter
      spaces
      case name of
        "/" -> p_go dirs []
        ".." -> p_go dirs (init cwd)
        name -> p_go dirs (cwd ++ [name])
    p_ls dirs cwd = do
      text "$ ls"
      spaces
      entries <- some $ do
        size <- (text "dir" >> pure Nothing) <|> (Just <$> p_nat)
        spaces
        name <- someText (letter <|> char '.')
        spaces
        return (name, size)
      p_go (dirs ++ [LS cwd entries]) cwd
    p_end dirs cwd = do
      return dirs

part1 input = sum [size | (path, size) <- Map.toList dirs, size <= 100000]
  where
    children = Map.fromList [(cwd, [cwd ++ [name] | (name, size) <- entries]) | LS cwd entries <- input]
    sizes = Map.fromList [(cwd ++ [name], size) | LS cwd entries <- input, (name, Just size) <- entries]
    dirs = flip fmap children $ \cs ->
      sum [fromJust (Map.lookup c sizes <|> Map.lookup c dirs) | c <- cs]

part2 input = minimum [size | (path, size) <- Map.toList dirs, size >= need]
  where
    need = (dirs Map.! []) - (70000000 - 30000000)
    children = Map.fromList [(cwd, [cwd ++ [name] | (name, size) <- entries]) | LS cwd entries <- input]
    sizes = Map.fromList [(cwd ++ [name], size) | LS cwd entries <- input, (name, Just size) <- entries]
    dirs = flip fmap children $ \cs ->
      sum [fromJust (Map.lookup c sizes <|> Map.lookup c dirs) | c <- cs]
