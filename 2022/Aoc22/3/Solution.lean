import Std
import Aoc22.Utils

open Lean

def input := include_str "input"

/- #eval 'a'.toNat -/
/- #eval 'A'.toNat -/

def priority(char: Char): Nat := 
  let char := char.toNat
  let a := 'a'.toNat
  let A := 'A'.toNat

  if char > a then 
    char - a + 1
  else 
    char - A + 27

def parse_p2_input(input: String): List (List Nat) := 
  input.splitOn "\n"
    |> .dropLast
    |> .map String.toList
    |> .map (List.map priority)

def parse_p1_input(input: String): List (List Nat × List Nat) := 
   parse_p2_input input
    |> .map List.halves

def common_elements: List Nat × List Nat → List Nat 
| ⟨ l, r ⟩ => (List.inter l r).eraseDup

def part1(input: String): Nat := 
  parse_p1_input input
    |> .map common_elements
    |> .map .sum
    |> .sum

def resolve_badge:  List Nat × List Nat × List Nat → Nat 
| ⟨ a, b, c ⟩ => 
  List.inter a b 
    |> .inter c 
    |> .eraseDup
    |> .sum

def part2(input: String): Nat := 
  parse_p2_input input
    |> List.triplets
    |> .map resolve_badge
    |> .sum

#eval part1 input
#eval part2 input
