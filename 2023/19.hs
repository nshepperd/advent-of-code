{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Linear.V2
import           Linear.Vector
import           Numeric.Search.Range
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators (eof)
import qualified Text.Trifecta as Trifecta

import           Search
import           Util

instance a ~ () => IsString (Parser a) where
  fromString str = spaced (T.pack str)

spaced :: Text -> Parser ()
spaced pat = void $ traverse (\c -> if c == ' ' then spaces else void $ char c) (T.unpack pat)

type Item = Map Char Int

input, sample :: (Map Text [(Maybe (Char,Char,Int), Text)], [Item])
[input, sample] = fmap (parse (p <* eof) . unsafePerformIO . T.readFile) ["input/19.txt", "sample/19.txt"]
  where
    p = do
      rules <- p_rules
      items <- p_items
      return (rules, items)
    p_cond = do
      prop <- oneOf "xmas"
      cmp <- oneOf "><"
      val <- p_nat
      ":"
      pure (prop, cmp, val)
      -- return $ case cmp of
      --   '>' -> \m -> (m Map.! prop) > val
      --   '<' -> \m -> (m Map.! prop) < val

    p_rule = do
      cond <- optional (Trifecta.try p_cond)
      target <- letters
      optional ","
      return (cond, target)
    p_rules = fmap Map.fromList . some $ do
      name <- letters
      rules <- "{" *> some p_rule <* "} "
      return (name, rules)
    p_items = some $ do
      "{"
      vals <- some $ do
        prop <- oneOf "xmas"
        "="
        val <- p_nat
        optional ","
        return (prop, val)
      "} "
      return (Map.fromList vals)

part1 (rules, items) = sum (map (go "in") items)
  where
    go "R" item = 0
    go "A" item = sum item
    go label item = foldr f (error "fallthrough") (rules Map.! label)
      where
        f (Nothing, new) k = go new item
        f (Just (prop,cmp,val), new) k = case cmp of
          '>' | item Map.! prop > val -> go new item
          '<' | item Map.! prop < val -> go new item
          _ -> k

part2 (rules, _) = sum $ map process $ go "in" []
  where
    process conds = foldr addrule finish conds (Map.fromList [(c,1) | c <-"xmas"]) (Map.fromList [(c,4000) | c <-"xmas"])
    addrule (prop, '>', val) k mins maxs = k (Map.insertWith max prop (val+1) mins) maxs
    addrule (prop, '≥', val) k mins maxs = k (Map.insertWith max prop val mins) maxs
    addrule (prop, '<', val) k mins maxs = k mins (Map.insertWith min prop (val-1) maxs)
    addrule (prop, '≤', val) k mins maxs = k mins (Map.insertWith min prop val maxs)
    finish mins maxs = product [if mx >= mn then mx-mn + 1 else 0 | c <- "xmas", let { mn = mins Map.! c; mx = maxs Map.! c }]
    go "R" conds = []
    go "A" conds = [conds]
    go label conds = foldr f (error "fallthrough") (rules Map.! label) conds
      where
        f (Nothing, new) k conds = go new conds
        f (Just (prop,cmp,val), new) k conds = case cmp of
          '>' -> go new ((prop,'>',val) : conds) <> k ((prop, '≤', val):conds)
          '<' -> go new ((prop,'<',val) : conds) <> k ((prop, '≥', val):conds)
