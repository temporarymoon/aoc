module Aoc.Day8 where

import Prelude

import Control.Alternative (guard)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Foldable (class Foldable, all, foldMap, foldl, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet (HashSet)
import Data.HashSet as HS
import Data.Hashable (class Hashable, hash)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines, words)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

---------- Types
data Segment
  = A
  | B
  | C
  | D
  | E
  | F
  | G

type Segments = HashSet Segment
type ConstraintSet = HashMap Segment Segments

type Input = Array (Array Segments /\ Array Segments)

type InferenceRule = Segments -> ConstraintSet
type InferenceSolution = HashMap Segment Segment

---------- Constants
-- | Fully lit display
allSegments :: Segments
allSegments = stringToSegment "abcdefg"

-- | Constraint set where everything can be everything
fullConstraintSet :: ConstraintSet
fullConstraintSet = allSegments
  # HS.map (_ /\ allSegments)
  # HM.fromFoldable

-- | Constraint set where everything can be nothing
emptyConstraintSet :: ConstraintSet
emptyConstraintSet = allSegments
  # HS.map (_ /\ HS.empty)
  # HM.fromFoldable

-- | Array containing all possible representations of digits
representations :: Array Segments
representations = buildRepresentations
  [ [ A, B, C, E, F, G ] -- 0
  , [ C, F ] -- 1
  , [ A, C, D, E, G ] -- 2
  , [ A, C, D, F, G ] -- 3
  , [ B, D, C, F ] -- 4
  , [ A, B, D, F, G ] -- 5
  , [ A, B, D, E, F, G ] -- 6
  , [ A, C, F ] -- 7
  , HS.toArray allSegments -- 8
  , [ A, B, C, D, F, G ] -- 9
  ]
  where
  buildRepresentations = map HS.fromArray

---------- Helpers
-- | My algorithm was broken, so I made this hacky function to fix it
fixBrokenSolution :: ConstraintSet -> ConstraintSet
fixBrokenSolution broken = foldlWithIndex go broken broken
  where
  go key previous current = intersectConstraintSets previous
    case HS.toArray current of
      [ solution ] -> birestrict (invertSegments $ HS.singleton key)
        (invertSegments $ HS.singleton solution)
      other -> fullConstraintSet

-- | Attempt to recognize the value a set of segments are displaying
displayValue :: Segments -> Maybe Int
displayValue segments = Array.findIndex ((==) segments) representations

invertSegments :: Segments -> Segments
invertSegments = HS.difference allSegments

-- | A constraint set is valid if each segment has at least one solution
isValidConstraintSet :: ConstraintSet -> Boolean
isValidConstraintSet = all (HS.isEmpty >>> not)

-- | Interesects the constrains inposed by 2 mappings
intersectConstraintSets :: ConstraintSet -> ConstraintSet -> ConstraintSet
intersectConstraintSets a b = HM.unionWith HS.intersection a b

-- | For each segment, allows either of the given constraint sets to be true 
unionConstraintSets :: ConstraintSet -> ConstraintSet -> ConstraintSet
unionConstraintSets a b = HM.unionWith HS.union a b

-- | Same as `unionConstraintSets`, but with n inputs
unionManyConstraintSets :: forall f. Foldable f => f ConstraintSet -> ConstraintSet
unionManyConstraintSets = foldl unionConstraintSets emptyConstraintSet

-- | Same as `unionConstraintSets`, but with n inputs
intersectManyConstraintSets :: forall f. Foldable f => f ConstraintSet -> ConstraintSet
intersectManyConstraintSets = foldl intersectConstraintSets fullConstraintSet

-- | Restrict a bunch of segments to a set of possibilites
restrict :: Segments -> Segments -> ConstraintSet
restrict toRestrict possibilities =
  toRestrict
    # HS.map (_ /\ possibilities)
    # HM.fromFoldable

-- | Restrict some segments to be part of a set of possibilities
-- | Adds an additional restriction of nothing else being part of that set
birestrict :: Segments -> Segments -> ConstraintSet
birestrict toRestrict possibilities =
  intersectConstraintSets additionalConstraint (restrict toRestrict possibilities)
  where
  additionalConstraint = toRestrict
    # invertSegments
    # HS.map (_ /\ remainingSegments)
    # HM.fromFoldable

  remainingSegments = invertSegments possibilities

---------- Debug stuff
showConstraintSet :: ConstraintSet -> String
showConstraintSet =
  HM.toArrayBy
    ( \k v -> show k <> ": " <> foldMap show v
    )
    >>> String.joinWith "\n"

---------- Parsing
stringToSegment :: String -> Segments
stringToSegment = toCharArray >>> mapMaybe charToSegment >>> HS.fromArray
  where
  charToSegment 'a' = Just A
  charToSegment 'b' = Just B
  charToSegment 'c' = Just C
  charToSegment 'd' = Just D
  charToSegment 'e' = Just E
  charToSegment 'f' = Just F
  charToSegment 'g' = Just G
  charToSegment _ = Nothing

parse :: String -> Input
parse = lines >>> mapMaybe \line -> case String.split (String.Pattern "|") line of
  [ input, output ] -> Just (parseSegments input /\ parseSegments output)
  _ -> Nothing
  where
  parseSegments = words
    >>> map stringToSegment
    >>> Array.filter (HS.isEmpty >>> not)

---------- Solutions
part1 :: Input -> Int
part1 puzzleInput =
  puzzleInput
    # map snd
    # map go
    # sum
  where
  go = map HS.size
    >>> Array.filter isEasyNumber
    >>> Array.length

  isEasyNumber l = l == 2 || l == 3 || l == 4 || l == 7

-- | Infer a set of constraints from an attempt at rendering a segment
inferConstraints :: Segments -> ConstraintSet
inferConstraints input = representations
  # Array.mapMaybe representationToConstraints
  # Array.filter isValidConstraintSet
  # unionManyConstraintSets
  where
  inputSize = HS.size input
  representationToConstraints representation = ado
    guard (HS.size representation == inputSize)
    in birestrict input representation

constraintsFromDataSet :: Array Segments -> ConstraintSet
constraintsFromDataSet = map inferConstraints >>> intersectManyConstraintSets

extractSolution :: ConstraintSet -> Maybe InferenceSolution
extractSolution = map HS.toArray >>> traverse case _ of
  [ mapping ] -> Just mapping
  _ -> Nothing

inferDisplayedValue :: Array Segments /\ Array Segments -> Maybe Int
inferDisplayedValue (input /\ output) =
  case
    extractSolution $ fixBrokenSolution $ constraintsFromDataSet (input <> output)
    of
    Just solution ->
      output
        # map (HS.mapMaybe (flip HM.lookup solution))
        # Array.mapMaybe displayValue
        # Array.foldMap show
        # Int.fromString
    Nothing -> Nothing

part2 :: Input -> Maybe Int
part2 = traverse inferDisplayedValue >>> map sum

---------- Running the solutions
main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/8.txt"
  let input = parse content
  logShow $ part1 input
  logShow $ part2 input

---------- Typeclass instances
derive instance Generic Segment _
derive instance Eq Segment
instance Hashable Segment where
  hash s = hash case s of
    A -> 0
    B -> 1
    C -> 2
    D -> 3
    E -> 4
    F -> 5
    G -> 6

instance Show Segment where
  show = genericShow