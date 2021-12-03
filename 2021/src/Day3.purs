module Aoc.Day3 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int (binary, pow)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.String.Utils (lines)
import Data.Typelevel.Num (D12)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type BitArray = Vec D12 Boolean

parse :: String -> Array BitArray
parse = lines
  >>> Array.mapMaybe
    ( toCharArray
        >>> Array.mapMaybe charToBool
        >>> Vec.fromArray
    )
  where
  charToBool '0' = Just false
  charToBool '1' = Just true
  charToBool _ = Nothing

toBit :: Char -> Int -> Char
toBit default x
  | x > 0 = '1'
  | x < 0 = '0'
  | otherwise = default

bitToSign :: Boolean -> Int
bitToSign bit = if bit then 1 else -1

boolToChar :: Boolean -> Char
boolToChar true = '1'
boolToChar false = '0'

part1 :: Array BitArray -> Maybe Int
part1 = map (map bitToSign)
  >>> sum
  >>> map (toBit '0')
  >>> Array.fromFoldable
  >>> String.fromCharArray
  >>> Int.fromStringAs binary
  >>> map \n -> n * (2 `pow` 12 - 1 - n)

part2Impl :: Boolean -> Array (Array Boolean) -> Maybe Int
part2Impl bias = go 0
  where
  go index arr
    | index >= 12 = Nothing
    | otherwise =
        if Array.length remaining == 1 then
          do
            first <- Array.head remaining
            first
              # map boolToChar
              # String.fromCharArray
              # Int.fromStringAs binary
        else
          go (index + 1) remaining
        where
        { no, yes } = arr # Array.partition \bits -> Array.index bits index
          == Just true

        remaining = case Array.length yes, Array.length no of
          la, lb
            | la >= lb -> if bias then yes else no
            | otherwise -> if bias then no else yes

part2 :: Array (Array Boolean) -> Maybe Int
part2 arr = ado
  oxygen <- part2Impl true arr
  co2 <- part2Impl false arr
  in oxygen * co2

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/3.txt"
  logShow $ part1 $ parse content
  logShow $ part2 $ map Array.fromFoldable $ parse content