open System.IO



module Day3A =
    type Direction = Horizontal | Vertical
    type Instruction = Direction * int
    type Point = int * int
    type WireConfigration = (Instruction array) * (Instruction array)

    let parseDirection (input: string) = (input.[0], int input.[1..(input.Length - 1)])
    let buildDirection (d: char, s: int) = 
        match d with 
            | 'R' -> (Horizontal, s)
            | 'U' -> (Vertical, -s)
            | 'L' -> (Horizontal, -s)
            | 'D' -> (Vertical, s)
            | _ -> failwith <| "Cannot parse direction" + string d
    let parseAndBuildDirection (input: string) = input |> parseDirection |> buildDirection

    let manhattanDistance ((x, y): Point) = abs x + abs y

    let sign x = x / abs x

    let rec unfoldDirection ((x0, y0):Point) ((d, s): Instruction) = 
        let stepsLeft = s - sign s
        

        match d with 
            | Horizontal -> 
                let point = (x0 + sign s, y0)
                point :: if stepsLeft = 0 then [] else unfoldDirection point (Horizontal, stepsLeft) 
            | Vertical -> 
                let point = (x0, y0 + sign s)
                point :: if stepsLeft = 0 then [] else unfoldDirection point (Vertical, stepsLeft) 

    let notOrigin ((x, y): Point) = (x <> 0) && (y <> 0)

    let parseConfiguration (input: string) = 
        let parsed = Array.map (fun (v: string) -> Array.map parseAndBuildDirection <| v.Split ",") <| input.Split "\n"
        (parsed.[0], parsed.[1])
    let unfoldConfiguration ((a,b): WireConfigration) =
        let unfoldMany (arr: Instruction array) = Array.fold (fun a o -> List.concat (seq { a; unfoldDirection (List.last a) o })) [(0,0)] arr
        (unfoldMany a, unfoldMany b)
    let findIntersections ((a, b):  Point list * Point list) = 
        Set.intersect <| Set.ofList a <| Set.ofList b |> Set.toList |> List.filter notOrigin
    let findMinimumIntersection (points: Point list) = 
        List.map <| manhattanDistance <| points |> List.min

    let solve input = input  |> parseConfiguration |> unfoldConfiguration |> findIntersections |> findMinimumIntersection

module Day3B = 
    let getLength (_default: int) ((a, b):  Day3A.Point array * Day3A.Point array) (index1: int)  = 
        if a.[index1] = (0,0) then 
            _default
        else
            try 
                let index2 = Array.findIndex ((=) a.[index1]) b

                printfn "%f" <| (float index1 / float a.Length) * 100.0
            
                let r = index2 + index1

                printfn "%i" r

                r
            with      
                | _ ->  _default

    let findMinimumIntersection ((a, b):  Day3A.Point list * Day3A.Point list) = 
        let aArray = Array.ofList a
        let bArray = Array.ofList b

        let r = Array.map <| getLength (a.Length + b.Length)   (aArray,bArray) <| [|0..a.Length|] 
        Array.min r

    let solve input = input  |> Day3A.parseConfiguration |> Day3A.unfoldConfiguration |> findMinimumIntersection




[<EntryPoint>]
let main argv =
    let text = File.ReadAllText argv.[0]
    // Part 1
    // let result = text |> Day3A.solve

    // Part 2
    let result = text |> Day3B.solve

    printfn "%A" result

    0 // return an integer exit code
