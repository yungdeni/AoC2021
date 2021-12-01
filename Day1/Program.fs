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
        File.ReadAllLines("./input.txt")
        |> Array.map int
    let Part1 =
        pair input |> Array.map findIncreases |> Array.sum
    let Part2 = threeMeasurement input
    printfn "Part 1: %i" Part1
    printfn ""
    printfn "Part 2: %i" Part2
    0
