import Std.Data.List.Basic

def List.sum [Add α] [OfNat α 0] : List α → α := 
  List.foldr Add.add 0

def Array.sum [Add α] [OfNat α 0] : Array α → α := 
  fun L => L.toList.sum

def List.triplets: List α →  List (α × α × α)
| a :: b :: c :: remaining => ⟨a, b, c⟩ :: triplets remaining
| _ => List.nil

def List.halves(input: List α): List α × List α := 
  input.splitAt (input.length / 2)
