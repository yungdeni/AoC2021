open System
open System.IO

let pair input = input |> Array.pairwise

let findIncreases (a, b) =
    match a, b with
    | a, b when a < b -> 1
    | _ -> 0

let threeMeasurement input =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> pair
    |> Array.sumBy findIncreases

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
        |> Array.map int
    let part1 =
        pair input |> Array.sumBy findIncreases 
    let part2 = threeMeasurement input
    printfn "Part 1: %i" part1
    printfn ""
    printfn "Part 2: %i" part2
    0
