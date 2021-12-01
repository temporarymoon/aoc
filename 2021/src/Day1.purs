module Aoc.Day1 where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

part1 :: List Int -> Int
part1 (a : b : rest) = (if a < b then 1 else 0) + part1 (b : rest)
part1 _ = 0

part2 :: List Int -> Int
part2 (a : rest@(_ : _ : d : _)) = (if a < d then 1 else 0) + part2 rest
part2 _ = 0

solutionST :: Int -> Array Int -> Int
solutionST offset array = ST.run do
  out <- STRef.new 0
  ST.for 0 (Array.length array - offset) \i -> do
    when (Array.index array i < Array.index array (i + offset)) do
      void $ STRef.modify ((+) 1) out
  STRef.read out

solutionNice :: Int -> Array Int -> Int
solutionNice offset arr = Array.zipWith (<) arr (Array.drop offset arr)
  # map fromEnum
  # sum

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/1.txt"
  let ints = lines content # Array.mapMaybe Int.fromString
  let intList = ints # List.fromFoldable
  logShow $ part1 intList
  logShow $ part2 intList
  logShow $ solutionST 3 ints
  logShow $ solutionNice 3 ints