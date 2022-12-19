{-# Language ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text.IO as T
import           Debug.Trace
import           Linear
import           Linear.V4
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           AStar
import           Util

input, sample :: [V4 (V4 Int)]
[input, sample] = fmap (parse p . unsafePerformIO . T.readFile) ["input/19.txt", "sample/19.txt"]
  where
    p = some $ do
      text "Blueprint " *> p_nat <* char ':' <* spaces
      ores <- some $ do
        text "Each "
        name <- someText letter
        text " robot costs "
        reqs <- (`sepBy` text "and ") $ do
          amt <- p_nat <* spaces
          someText letter <* spaces
          return amt
        char '.' <* spaces
        return reqs
      let [[ore_ore], [clay_ore], [obs_ore, obs_clay], [geode_ore, geode_obs]] = ores
      return (V4
              (V4 ore_ore 0 0 0)
              (V4 clay_ore 0 0 0)
              (V4 obs_ore obs_clay 0 0)
              (V4 geode_ore 0 geode_obs 0))

part1 input = sum [i * solve reqs 24 | (i,reqs) <- zip [1..] input]
part2 input = product [solve reqs 32 | reqs <- take 3 input]

vge :: V4 Int -> V4 Int -> Bool
vge a b = and (liftA2 (>=) a b)

solve reqs time = simple s0 - getSum (astar actions step goal s0 output)
  where
    -- simple (res, bots, t)
    --   | t == 0 = toList res !! 3
    --   | otherwise = simple (res + bots, bots + sum [ix | (ix, req) <- zip basis (toList reqs), res `vge` req], t-1)
    simple (res, bots, t) = simple' (pure res, bots, t)
    simple' (vres, bots, t)
      | t == 0 = toList (toList vres !! 3) !! 3
      | otherwise = simple' (vres', bots', t-1)
      where
        addbots = fromEnum <$> (vge <$> vres <*> reqs) :: V4 Int
        used = (pure <$> addbots) * reqs
        vres' = vres + pure bots - used
        bots' = bots + addbots

    actions (res, bots, t) = [0] ++ [ix | (ix, req) <- zip basis (toList reqs), and (liftA2 (>=) res req)] :: [V4 Int]
    step (res, bots, t) ix = (st', simple (res, bots, t) - simple st')
      where
        req = ix *! reqs
        st' = (res + bots - req, bots + ix, t-1)
    goal (res, bots, t)
      | (t == 0) = 0 -- traceShow t
      | t > 0 = 1
    s0 = (0, V4 1 0 0 0, time)
    output s a n c = (Sum c)

main :: IO ()
main = do print ("yay", part2 input)
