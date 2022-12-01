import Std
import Aoc22.Utils

abbrev ParsedInput := List (List Nat) 

#check (String.splitOn "234" "234" )
#check List.toArray

def input := include_str "input"
def parse_input(input: String): ParsedInput := 
  input.splitOn "\n\n"
   |> .map fun chunk => 
     chunk 
      |> (String.splitOn · "\n")
      |> .map String.toNat!

def totals(input: ParsedInput): List Nat := 
 input.map .sum

def part1(input: String): Option Nat := input 
   |> parse_input
   |> totals
   |> List.maximum?

def part2(input: String) := 
   let totals := input 
      |> parse_input
      |> totals

   let sorted := totals.toArray.qsort (· ≥ · )

   sorted[0:3].toArray.sum

#eval part1 input
#eval part2 input
