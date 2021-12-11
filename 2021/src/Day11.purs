module Aoc.Day11 where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldMap, (..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as String
import Data.String.Utils as SU
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

---------- Types
type Input = Array (Array Int)
type Position = Int /\ Int
type OctoMap = Position -> Int
type OctoMetadata = Int /\ Int

---------- Parsing
parse :: String -> Input
parse = String.trim
  >>> SU.lines
  >>> map (SU.toCharArray >>> Array.mapMaybe Int.fromString)

extractMetadata :: Input -> OctoMetadata
extractMetadata = Array.length &&& (Array.head >>> maybe 0 Array.length)

createOctoMap :: Input -> OctoMap
createOctoMap input (x /\ y) = fromMaybe 0 do
  row <- Array.index input x
  Array.index row y

---------- Solution
-- | Only here because of INFINITE RECURSION ERRORS AAAAAAAAAAAAA
flushOctoMap :: OctoMetadata -> OctoMap -> OctoMap
flushOctoMap metadata = octoMapToInput metadata >>> createOctoMap

-- | The inverse of createOctoMap
octoMapToInput :: OctoMetadata -> OctoMap -> Input
octoMapToInput (width /\ height) octoMap = Array.replicate width (Array.replicate height 0)
  # Array.mapWithIndex \x -> Array.mapWithIndex
      \y _ -> octoMap (x /\ y)

-- | Generate an array with all the possible positions on the grid
positions :: OctoMetadata -> Array Position
positions (width /\ height) = ado
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  in x /\ y

-- | Count the number of flashes which occured last tick
countFlashes :: OctoMetadata -> OctoMap -> Int
countFlashes metadata octoMap = Array.length do
  x /\ y <- positions metadata
  guard (octoMap (x /\ y) == 0)

-- | Sets all overflowing power levels to 0
resetPowerLevels :: OctoMap -> OctoMap
resetPowerLevels map position = do
  let current = map position
  if current > 9 then 0 else current
  where
  foo = 1 /\ 2

-- | Create a list with the immediate negihbours of a cell
neighbours :: Position -> Array Position
neighbours (x /\ y) = do
  offsetX <- (-1) .. 1
  offsetY <- (-1) .. 1
  guard (offsetX /= 0 || offsetY /= 0)
  pure $
    (x + offsetX) /\ (y + offsetY)

-- | Increase the amount of power an octopus holds, by 1
-- | Does nothing if the power has been reset (aka has a value of 0)
increasePower :: Position -> OctoMap -> OctoMap
increasePower at octoMap current
  | current == at = do
      let power = octoMap current
      if power == 0 then power else power + 1
  | otherwise = octoMap current

-- | Attempt to flash a cell. Does nothing if the power level is not greater than 9
attemptFlash :: Position -> OctoMap -> OctoMap
attemptFlash at octoMap
  | octoMap at > 9 = flash at octoMap
  | otherwise = octoMap

-- | Sets the power value of a cell at a specific position
setCell :: Position -> Int -> OctoMap -> OctoMap
setCell at value octoMap current
  | at == current = value
  | otherwise = octoMap current

-- | Perform a flash at a specific position
flash :: Position -> OctoMap -> OctoMap
flash at@(fx /\ fy) octoMap =
  foldl (#) (setCell at 0 octoMap) powerIncreases
  where
  powerIncreases = ado
    neighbour <- neighbours at
    in
      \octoMap -> do
        attemptFlash neighbour
          $ increasePower neighbour octoMap

flashAll :: OctoMetadata -> OctoMap -> OctoMap
flashAll metadata octoMap = foldl (#) octoMap flashes
  where
  flashes = map attemptFlash (positions metadata)

showOctoMap :: OctoMetadata -> OctoMap -> String
showOctoMap metadata = octoMapToInput metadata >>> map (foldMap show)
  >>> String.joinWith "\n"

simulate :: OctoMetadata -> OctoMap -> OctoMap
simulate metadata =
  increaseAll
    >>> flashAll metadata
    >>> flushOctoMap metadata
  where
  increaseAll = map ((+) 1)

part1 :: OctoMetadata -> Int -> OctoMap -> Int
part1 metadata amount octoMap
  | amount <= 0 = 0
  | otherwise =
      octoMap
        # simulate metadata
        # (identity &&& countFlashes metadata)
        # uncurry \octoMap flashes ->
            flashes + part1 metadata (amount - 1) octoMap

part2 :: OctoMetadata -> OctoMap -> Int
part2 metadata octoMap = do
  let updated = simulate metadata octoMap
  if octoMapToInput metadata updated # join # Array.all ((==) 0) then 1
  else 1 + part2 metadata updated

---------- Running the solutions
main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/11.txt"
  let input = parse content
  let metadata = extractMetadata input
  let octoMap = createOctoMap input
  logShow $ part1 metadata 100 octoMap
  logShow $ part2 metadata octoMap
