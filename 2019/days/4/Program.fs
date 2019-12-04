open System

module Day1A = 
    let rec checkDigits (x: int) (last: int) (foundDuplicates: bool) =
        if x = 0 then
            foundDuplicates
        else
            let lastDigit = x % 10
            let hasDuplicates = last = lastDigit || foundDuplicates

            lastDigit <= last && (checkDigits <| x / 10 <| lastDigit <| hasDuplicates)

    let boolToInt (i: bool) = if i  then 1 else 0

    let rec solve (start: int) (_end: int) (count: int) = 
        if start <> _end then
            let currentDiff = checkDigits start 10 false |> boolToInt
            let partialResult = currentDiff + count
            
            solve <| start + 1 <| _end <| partialResult
        else 
            count



[<EntryPoint>]
let main argv =
    let (start, _end) = (int argv.[0],int argv.[1])

    // Part 1
    let result = Day1A.solve start _end 0

    printfn "%i" result
    0 // return an integer exit code
