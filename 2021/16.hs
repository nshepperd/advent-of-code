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
import           Data.Monoid
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
import           Numeric.Search.Range
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           AStar
import           Util
-- import qualified Util.Text as T

input, sample :: String
input = "40541D900AEDC01A88002191FE2F45D1006A2FC2388D278D4653E3910020F2E2F3E24C007ECD7ABA6A200E6E8017F92C934CFA0E5290B569CE0F4BA5180213D963C00DC40010A87905A0900021B0D624C34600906725FFCF597491C6008C01B0004223342488A200F4378C9198401B87311A0C0803E600FC4887F14CC01C8AF16A2010021D1260DC7530042C012957193779F96AD9B36100907A00980021513E3943600043225C1A8EB2C3040043CC3B1802B400D3CA4B8D3292E37C30600B325A541D979606E384B524C06008E802515A638A73A226009CDA5D8026200D473851150401E8BF16E2ACDFB7DCD4F5C02897A5288D299D89CA6AA672AD5118804F592FC5BE8037000042217C64876000874728550D4C0149F29D00524ACCD2566795A0D880432BEAC79995C86483A6F3B9F6833397DEA03E401004F28CD894B9C48A34BC371CF7AA840155E002012E21260923DC4C248035299ECEB0AC4DFC0179B864865CF8802F9A005E264C25372ABAC8DEA706009F005C32B7FCF1BF91CADFF3C6FE4B3FB073005A6F93B633B12E0054A124BEE9C570004B245126F6E11E5C0199BDEDCE589275C10027E97BE7EF330F126DF3817354FFC82671BB5402510C803788DFA009CAFB14ECDFE57D8A766F0001A74F924AC99678864725F253FD134400F9B5D3004A46489A00A4BEAD8F7F1F7497C39A0020F357618C71648032BB004E4BBC4292EF1167274F1AA0078902262B0D4718229C8608A5226528F86008CFA6E802F275E2248C65F3610066274CEA9A86794E58AA5E5BDE73F34945E2008D27D2278EE30C489B3D20336D00C2F002DF480AC820287D8096F700288082C001DE1400C50035005AA2013E5400B10028C009600A74001EF2004F8400C92B172801F0F4C0139B8E19A8017D96A510A7E698800EAC9294A6E985783A400AE4A2945E9170"
sample = "A0016C880162017C3686B18A3D4780"

binary :: String -> Int
binary xs = foldl (\n digit -> case digit of
                      '0' -> n * 2
                      '1' -> n * 2 + 1) 0 xs

toBinary :: String -> String
toBinary hex = foldMap hexDigit hex
  where
    hexDigit '0' = "0000"
    hexDigit '1' = "0001"
    hexDigit '2' = "0010"
    hexDigit '3' = "0011"
    hexDigit '4' = "0100"
    hexDigit '5' = "0101"
    hexDigit '6' = "0110"
    hexDigit '7' = "0111"
    hexDigit '8' = "1000"
    hexDigit '9' = "1001"
    hexDigit 'A' = "1010"
    hexDigit 'B' = "1011"
    hexDigit 'C' = "1100"
    hexDigit 'D' = "1101"
    hexDigit 'E' = "1110"
    hexDigit 'F' = "1111"

data Packet = Packet Int Int (Maybe Int) [Packet]
  deriving (Show)

p_bit = oneOf ['0', '1']
p_packet = do
  version <- binary <$> replicateM 3 p_bit
  typeid <- binary <$> replicateM 3 p_bit
  case typeid of
    4 -> do
      ones <- many (char '1' *> replicateM 4 p_bit)
      zero <- char '0' *> replicateM 4 p_bit
      let val = binary (fold ones ++ zero)
      return (Packet version typeid (Just val) [])
    _ -> do
      lengthtype <- p_bit
      case lengthtype of
        '0' -> do -- length in bits
          len <- binary <$> replicateM 15 p_bit
          contents <- replicateM len p_bit
          let subpackets = parseString (many p_packet) contents
          return (Packet version typeid Nothing subpackets)
        '1' -> do -- length in packets
          len <- binary <$> replicateM 11 p_bit
          subpackets <- replicateM len p_packet
          return (Packet version typeid Nothing subpackets)

part1 input = addVersions packets
  where
    addVersions (Packet v t lit subpackets) = v + sum (map addVersions subpackets)
    packets = parse p_packet (T.pack (toBinary input))

part2 input = eval packets
  where
    packets = parse p_packet (T.pack (toBinary input))
    eval (Packet _ 4 (Just val) []) = val
    eval (Packet _ 0 Nothing sub) = sum (eval <$> sub)
    eval (Packet _ 1 Nothing sub) = product (eval <$> sub)
    eval (Packet _ 2 Nothing sub) = minimum (eval <$> sub)
    eval (Packet _ 3 Nothing sub) = maximum (eval <$> sub)
    eval (Packet _ 5 Nothing [a, b]) = if eval a > eval b then 1 else 0
    eval (Packet _ 6 Nothing [a, b]) = if eval a < eval b then 1 else 0
    eval (Packet _ 7 Nothing [a, b]) = if eval a == eval b then 1 else 0
