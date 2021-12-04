module Aoc.Day4 where

import Prelude

import Control.Alternative (guard)
import Data.Array (range)
import Data.Array as Array
import Data.Foldable (for_, sum)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..), swap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

---------- Types
type NumberDeck = Array Int
type Board = Array (Array Int)
type Input = NumberDeck /\ Array Board

---------- Parsing
parse :: String -> Maybe Input
parse = String.trim
  >>> String.split (String.Pattern "\n\n")
  >>> Array.uncons
  >>> map go
  where
  go { head, tail } = do
    let deck = parseDeck head
    let boards = map parseBoard tail
    deck /\ boards

  parseDeck = String.split (String.Pattern ",")
    >>> Array.mapMaybe Int.fromString

  parseBoard = lines
    >>> map
      (words >>> Array.mapMaybe Int.fromString)

---------- Solutions
-- | Checks if an individual cell is marked on the board
isMarked :: HashSet Int -> Int /\ Int -> Board -> Boolean
isMarked marked (x /\ y) board = fromMaybe false do
  column <- Array.index board x
  cell <- Array.index column y
  pure $ HashSet.member cell marked

-- | Takes a position generator function and tests if all the positions returned 
-- | by applying the function to ints between 0 and 4 are marked
markedInARow :: HashSet Int -> Array (Int /\ Int) -> Board -> Boolean
markedInARow marked positions board = positions
  # Array.all \position -> isMarked marked position board

-- | Checks whether a board is in a winning state
isWinner :: HashSet Int -> Board -> Boolean
isWinner marked board = winConditions
  # Array.any
      \condition -> markedInARow marked condition board
        || markedInARow marked (map swap condition) board
  where
  winConditions = ado
    x <- range 0 4
    in map (Tuple x) (range 0 4)

-- | Attempts to calculate the score of a winning board.
-- | returns Nothing is the board is not in a winning state
score :: HashSet Int -> Board -> Maybe Int
score marked board = do
  guard (isWinner marked board)
  board
    # join
    # Array.filter isScored
    # sum
    # Just
  where
  isScored cell = not (HashSet.member cell marked)

-- | Finds the solution to the aoc problem.
-- | Pass true for part 1 and false for part 2
solution :: Boolean -> Input -> Int
solution shouldWin (deck /\ boards) = go boards 1
  where
  go boards depth = case winner of
    Nothing -> go boards (depth + 1)
    Just (index /\ score)
      | shouldWin || Array.length boards == 1
      , Just last <- Array.index deck (depth - 1) ->
          last * score
      | otherwise ->
          go remainingBoards depth
          where
          remainingBoards = fromMaybe boards
            (Array.deleteAt index boards)
    where
    winner = boards
      # Array.mapWithIndex (/\)
      # Array.findMap \(index /\ board) -> score marked board
          # map (Tuple index)
    marked = HashSet.fromFoldable
      (Array.take depth deck)

---------- Running the solutions
main :: Effect Unit
main = do
  content <- readTextFile UTF8 "inputs/4.txt"
  for_ (parse content) \input -> do
    logShow $ solution true input
    logShow $ solution false input