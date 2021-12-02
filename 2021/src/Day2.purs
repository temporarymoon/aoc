module Aoc.Day2 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, words)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Move
  = Forward Int
  | Vertical Int

parse :: String -> Array Move
parse = lines >>> Array.mapMaybe \line -> case words line of
  [ command, s ] | Just spaces <- Int.fromString s -> case command of
    "up" -> Just $ Vertical (-spaces)
    "down" -> Just $ Vertical spaces
    "forward" -> Just $ Forward spaces
    _ -> Nothing
  _ -> Nothing

moveToVec :: Move -> Vec D2 Int
moveToVec (Forward x) = vec2 x 0
moveToVec (Vertical y) = vec2 0 y

multiplyCoords :: Vec D2 Int -> Int
multiplyCoords = \v -> Vec.head v * Vec.last v

part1 :: Array Move -> Int
part1 = map moveToVec >>> sum >>> multiplyCoords

part2 :: Array Move -> Int
part2 = Array.foldl go (zero /\ 0) >>> fst >>> multiplyCoords
  where
  go (position /\ aim) = case _ of
    Forward x -> (position + vec2 x (aim * x)) /\ aim
    Vertical y -> position /\ (aim + y)

-- | Solution to part 2 which never explicitly passes the aim around
part2NoAccum :: Array Move -> Int
part2NoAccum = Array.foldr go zero >>> multiplyCoords
  where
  go (Forward x) position = position + vec2 x 0
  go (Vertical y) position = position + vec2 0 (y * Vec.head position)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/2.txt"
  let vecs = parse content
  logShow $ part1 vecs
  logShow $ part2 vecs
  logShow $ part2NoAccum vecs