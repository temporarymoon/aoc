import Std.Data.List.Basic

def List.sum [Add α] [OfNat α 0] : List α → α := 
  List.foldr Add.add 0

def Array.sum [Add α] [OfNat α 0] : Array α → α := 
  fun L => L.toList.sum
