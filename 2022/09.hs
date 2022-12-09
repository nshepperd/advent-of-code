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
import           Control.Concurrent.Async
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
import           Data.IORef
import           Data.Int
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.RVar as R
import qualified Data.Random as R
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
import qualified System.Random as R
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Codec.Picture ( PixelRGBA8( .. ), writePng )
import qualified Graphics.Rasterific as G
import qualified Graphics.Rasterific.Texture as G
import           Graphics.Text.TrueType ( loadFontFile )
import           Text.Printf

import           Util

sample, input :: [(Char, Int)]
(sample, input) = over each (parse p . unsafePerformIO . T.readFile) ("sample/09.txt", "input/09.txt")
  where
    p = some $ do
      d <- letter
      spaces
      n <- p_nat
      spaces
      return (d, n)

v :: Char -> V2 Int
v 'U' = V2 0 1
v 'D' = V2 0 (-1)
v 'L' = V2 (-1) 0
v 'R' = V2 1 0

part1 input = foldr go finish stretched 0 0 []
  where
    stretched = concat [replicate n d | (d, n) <- input]
    go d k headPos tailPos visited = k headPos' tailPos' (visited ++ [tailPos'])
      where
        headPos' = headPos + v d
        tailPos' = case headPos' - tailPos of
          rel | maximum (abs rel) >= 2 -> tailPos + (signum <$> rel)
          _ -> tailPos
    finish headPos tailPos visited = Set.size $ Set.fromList visited --Set.size visited

part2 input = foldr go finish stretched (replicate 10 0) [replicate 10 0]
  where
    stretched = concat [replicate n d | (d, n) <- input]
    go d k knots visited = k knots' (visited ++ [knots'])
      where
        knots' = (head knots + v d) : [let rel = leader - follower in
                                         if maximum (abs rel) >= 2 then
                                           follower + (signum <$> rel)
                                         else follower
                                      | (leader, follower) <- zip knots' (tail knots)]
    finish knots visited = Set.size $ Set.fromList (map last visited)


traverseFast :: (a -> IO ()) -> [a] -> IO ()
traverseFast f xs = do
  q <- newTQueueIO
  done <- atomically $ newTVar 0
  let worker = forever $ do
        item <- atomically $ readTQueue q
        item
        atomically (modifyTVar done (+1))
  worker <- replicateM 16 (forkIO worker)
  traverse_ (\x -> atomically (writeTQueue q (f x))) xs
  atomically $ do
    v <- readTVar done
    when (v < length xs) retry

plot2 input = foldr go finish stretched (replicate 10 0) [replicate 10 0]
  where
    cw = 12
    ch = 20
    stretched = concat [replicate n d | (d, n) <- input]
    go d k knots visited = k knots' (visited ++ [knots'])
      where
        knots' = (head knots + v d) : [let rel = leader - follower in
                                         if maximum (abs rel) >= 2 then
                                           follower + (signum <$> rel)
                                         else follower
                                      | (leader, follower) <- zip knots' (tail knots)]
    canvasSize = V2 101 61
    canvasHalf = fmap (`div` 2) canvasSize
    V2 canvasSizeX canvasSizeY = canvasSize
    finish knots visited = do font <- either error id <$> loadFontFile "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
                              let corner = last (head visited) - canvasHalf
                                  corner' = corner * V2 cw ch
                                  cvisits = scanl (\cv knots -> Set.insert (last knots) cv) Set.empty visited
                                  cviewport = scanl (\p knots -> 0.9 * p + 0.1 * (fromIntegral <$> last knots)) (fromIntegral <$> (last $ head visited)) visited
                              traverseFast (display font) (zip4 [0..] visited cvisits cviewport)
    display font (i::Int, knots, visits, viewport) = do
      print i
      let V2 xmin ymin = (round <$> viewport) - canvasHalf
          V2 xmax ymax = canvasSize + (V2 xmin ymin)
      writePng (printf "demo/%d.png" i) . G.renderDrawing (canvasSizeX*cw) (canvasSizeY*ch) (PixelRGBA8 0 0 0 255) $ do
        flip traverse_ (concat [[(V2 x y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]) $ \p@(V2 x y) -> do
          let gen = R.mkStdGen (case p of V2 x y -> x * 1000 + y)
              (bgcolor, bg) = fst $ flip R.pureRVar gen $ do
                ch <- R.randomElement "`  ,   '           "
                color <- R.randomElement [PixelRGBA8 255 255 0 255,
                                          PixelRGBA8 128 255 0 255,
                                          PixelRGBA8 255 128 0 255]
                return (color, ch)
              loc' = ((fromIntegral <$> canvasHalf) + ((fromIntegral <$> p) - viewport) * (V2 1 (-1))) * (fromIntegral <$> V2 cw ch)
              loc = G.V2 (view _x loc') (view _y loc') :: G.Point
          case p of
            p | p == head knots -> G.withTexture (G.uniformTexture $ PixelRGBA8 255 255 255 255) $ do
                  G.printTextAt font (G.PointSize 16) loc "H"
            p | elem p knots -> G.withTexture (G.uniformTexture $ PixelRGBA8 255 255 255 255) $ do
                  G.printTextAt font (G.PointSize 16) loc "*"
            p | Set.member p visits -> G.withTexture (G.uniformTexture $ PixelRGBA8 128 0 0 255) $ do
                  G.printTextAt font (G.PointSize 16) loc "*"
            _ -> G.withTexture (G.uniformTexture bgcolor) $ do
                  G.printTextAt font (G.PointSize 16) loc [bg]
    -- finish knots visited = traverse_ display visited
    --   where
    --     display knots = threadDelay (20*1000) >> T.putStrLn (T.pack ("\x1b[2J" ++ (unlines $ reverse [concat [c knots (V2 x y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]))) >> hFlush stdout
    --       where
    --         -- crimp_low x = x - 5
    --         -- crimp_high x = x + 5
    --         -- V2 xmin ymin = crimp_low <$> foldl1 (liftA2 min) knots --(concat visited)
    --         -- V2 xmax ymax = crimp_high <$> foldl1 (liftA2 max) knots --(concat visited)
    --         V2 xmin ymin = (+ (-30)) <$> head knots
    --         V2 xmax ymax = (+ 30) <$> head knots
    --     c knots p | p == head knots = "H"
    --               | elem p knots = "*"
    --               | otherwise = background :: String
    --       where
    --         gen = R.mkStdGen (case p of V2 x y -> x * 1000 + y)
    --         background = fst $ R.pureRVar bg gen
    --         bg = do
    --           ch <- R.randomElement "`  "
    --           color <- R.randomElement [2,3,4,5]
    --           return ("\x1b[3" ++ show color ++ "m" ++ [ch] ++ "\x1b[m") :: R.RVar String


main :: IO ()
main = do
  plot2 input
  -- font <- either error id <$> loadFontFile "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
  -- writePng "demo.png" . G.renderDrawing 300 70 (PixelRGBA8 0 0 0 255) $ do
  --   G.withTexture (G.uniformTexture $ PixelRGBA8 255 255 0 255) $ do
  --     G.printTextAt font (G.PointSize 12) (G.V2 20 40) "A simple text test!"
  --     G.printTextAt font (G.PointSize 12) (G.V2 20 54) "A simple text test!"
