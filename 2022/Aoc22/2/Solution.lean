import Std
import Aoc22.Utils

abbrev ParsedInput := List (Nat × Nat) 

def input := include_str "input"

def parse_input(input: String): ParsedInput := 
  let lines := input.splitOn "\n" 
  let parse_line: List String → Option (Nat × Nat) := fun 
    | [ a, b ] => do
        let left <- match a with
          | "A" => some 0
          | "B" => some 1
          | "C" => some 2
          | _ => none

        let right <- match b with
          | "X" => some 0
          | "Y" => some 1
          | "Z" => some 2
          | _ => none

        pure ⟨left, right⟩
    | _ => none

  lines.dropLast
    |> .map (String.splitOn · " ")
    -- I could use filtermap, but I want to error out if something goes wrong with the input
    |> .map parse_line
    |> .map Option.get! 

def total_score(input: ParsedInput): Nat := 
  let score := fun
    | ⟨ l, r ⟩ => ((4 + r - l) % 3) * 3 + r + 1

  input 
   |> .map score
   |> .sum

def guess_moves(input: ParsedInput): ParsedInput := 
  input.map fun 
    -- This formula is the inverse of line 9
    | ⟨ l, o ⟩ => ⟨ l, (o + l + 2) % 3 ⟩ 

def part1(input: String): Nat := 
  total_score (parse_input input)

def part2(input: String): Nat := 
  total_score (guess_moves (parse_input input))

#eval part1 input
#eval part2 input
