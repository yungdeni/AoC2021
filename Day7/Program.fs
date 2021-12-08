open System.IO
let split (line:string) = 
    line.Split(",")

let triangularNum n = 
    (n * (n+1))/2

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
        |> Array.collect split
        |> Array.map int
    let sortedInput = 
        input
        |>Array.sort
    //Median
    let toHorzpart1 = 
        sortedInput.[sortedInput.Length/2]
    //Average
    let toHorzpart2 = 
        sortedInput
        |>Array.map float
        |>Array.average
        |> (floor >> int)

    let part1 = 
        input
        |> Array.fold (fun acc v -> (abs (v - toHorzpart1)) + acc) 0

    let part2 = 
        input
        |> Array.fold (fun acc v -> (triangularNum (abs (v - toHorzpart2))) + acc) 0
    printfn "Part1 : %i" part1
    printfn "Part2 : %i" part2
    0 // return an integer exit code