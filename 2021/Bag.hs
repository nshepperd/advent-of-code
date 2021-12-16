module Bag where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

newtype Bag k = Bag (Map k Int)

instance Ord k => Semigroup (Bag k) where
  (<>) (Bag a) (Bag b) = Bag (Map.unionWith (+) a b)

instance Ord k => Monoid (Bag k) where
  mempty = Bag Map.empty
  mappend = (<>)

singleton :: Ord k => k -> Bag k
singleton k = Bag (Map.singleton k 1)

fromList :: Ord k => [k] -> Bag k
fromList ks = Bag (Map.fromListWith (+) [(k,1) | k <- ks])

toMap :: Bag k -> Map k Int
toMap (Bag map) = map