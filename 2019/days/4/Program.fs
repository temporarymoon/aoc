open System

module Day4A = 
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


module Day4B = 
    let rec checkDigits (x: int) (last: int) (foundDuplicates: bool) (duplicateCount: int)=
        if x = 0 then
            foundDuplicates
        else
            let lastDigit = x % 10
            let nextX = x / 10
            let nextDigit = nextX % 10
            let willBeDuplicate = lastDigit = nextDigit

            let nextDuplicateCount = if willBeDuplicate then duplicateCount + 1 else 0
            let hasDuplicates = (nextDuplicateCount = 0 && duplicateCount = 1) || foundDuplicates 

            lastDigit <= last && (checkDigits nextX lastDigit hasDuplicates nextDuplicateCount)

    let rec solve (start: int) (_end: int) (count: int) = 
        if start <> _end then
            let currentDiff = checkDigits start 10 false 0 |> Day4A.boolToInt
            let partialResult = currentDiff + count
            
            solve <| start + 1 <| _end <| partialResult
        else 
            count

[<EntryPoint>]
let main argv =
    let (start, _end) = (int argv.[0],int argv.[1])

    // Part 1
    // let result = Day4A.solve start _end 0

    // Part 2
    let result = Day4B.solve start _end 0

    printfn "%i" result
    0 // return an integer exit code
