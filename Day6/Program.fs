open System.IO

let split (line:string) = 
    line.Split(",")
let addInputs (input:uint64[]) = 
    let timers = [|for i in 0 .. 9 -> 0UL|]
    for i in input do 
        timers.[int i] <- timers.[int i] + 1UL
    timers

let incrementFish (timers:uint64[]) = 
    let (a,b,c,d,e,f,g,h,i) = (timers.[1], timers.[2] ,timers.[3] ,timers.[4] ,timers.[5] ,timers.[6],(timers.[0] + timers.[7]),timers.[8], timers.[0])
    let res = [|a;b;c;d;e;f;g;h;i|]
    res
let solver (steps:int) (input:uint64[]) =
    let rec inner (steps:int) (input:uint64[]) = 
        match steps with
        |1 -> incrementFish input
        |_ -> inner (steps-1) (incrementFish input)
    inner steps input

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
        |> Array.collect split
        |> Array.map uint64
    let part1 = 
        input
        |> addInputs
        |> solver 80
        |> Array.sum
    let part2 = 
        input
        |> addInputs
        |> solver 256
        |> Array.sum
    printfn "Part1: %ul" part1
    printfn "Part2: %ul" part2
    0 // return an integer exit code