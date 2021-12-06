open System.IO
open System

let splitNewline (inp:string) = 
    inp.Split([|"\r\n\r\n "|], StringSplitOptions.RemoveEmptyEntries)

let splitSpace (inp:string) =  
    inp.Split(" ", StringSplitOptions.RemoveEmptyEntries)

let joinInto2D (listofint:int[] list) = 
  Array2D.init 5 5 (fun i j ->
    listofint.[i].[j])

//Put a -1 in place of a crossed number. kind of hacky
let crossOut num bingo = 
    bingo
    |> Array2D.map (fun v -> if v = num then -1 else v)
//if a row has a sum of -5 then we got bingo (a row has 5 numbers hardcoded...)
let checkIfBingo bingo = 
    match Array.sum bingo with
    | -5 -> true
    | _ -> false

let bingoFlatter (bingo:int[,]) = 
    let ret = [for i in 0..4 -> bingo.[*,i]]
    let res = [for i in 0..4 -> bingo.[i,*]]
    List.append ret res

let rec bingoChecker flatBingo = 
    match flatBingo with
    | [] -> false
    | head::tail ->
        if checkIfBingo head
            then true
            else bingoChecker tail
let takeJustBingos bingo = 
    let flatBingo = bingoFlatter bingo
    match bingoChecker flatBingo with
    | false -> None
    | true -> Some bingo
let takeNotBingos bingo = 
    let flatBingo = bingoFlatter bingo
    match bingoChecker flatBingo with
    | false -> Some bingo
    | true -> None

let playoneRoundBingo numberToDraw bingos = 
        bingos
        |> List.map (crossOut numberToDraw)
        // |> List.choose takeJustBingos
let playBingo (numbersToDraw:int list) bingos = 
    let rec inner (numbersToDraw:int list) bingos = 
        let bingoafterdraw = playoneRoundBingo numbersToDraw.[0] bingos
        let res = List.choose takeJustBingos bingoafterdraw
        match res  with
        |[] -> inner numbersToDraw.Tail bingoafterdraw
        |_ -> (Array2D.map (fun v -> if v = -1 then 0 else v) res.[0], numbersToDraw.[0])
    inner numbersToDraw bingos
let playNotBingo (numbersToDraw:int list) bingos = 
    let rec inner (numbersToDraw:int list) bingos = 
        let bingoafterdraw = playoneRoundBingo numbersToDraw.[0] bingos
        let res = List.choose takeNotBingos bingoafterdraw
        match res  with
        |res when res.Length = 1 -> playBingo numbersToDraw.Tail res
        |_ -> inner numbersToDraw.Tail bingoafterdraw

    inner numbersToDraw bingos


[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    let chosenNumbers = 
        input.[0].Split(",")
        |> Array.map int
        |> Array.toList
    let bingos = 
        input
        |> Array.skip 1
        |> Array.filter (fun i -> i <> "")
        |> Array.toList
        |> List.map splitSpace
        |> List.map (Array.map int)
        |> List.chunkBySize 5
        |> List.map joinInto2D

        // |>List.map (crossOut 7) 
    let part1bingo = 
        playBingo chosenNumbers bingos
    let winningBingo,winningnumber = part1bingo
    let part1sum = 
        Seq.cast<int> winningBingo
        |> Seq.sum 
    let part1 = part1sum * winningnumber

    let part2bingo = 
        playNotBingo chosenNumbers bingos
    let lastWinningBingo,lastWinningNumber = part2bingo
    let part2sum = 
        Seq.cast<int> lastWinningBingo
        |> Seq.sum 
    let part2 = part2sum * lastWinningNumber//    toJaggedArray bingos 5
    printfn "Part1: %i" part1
    printfn "Part2: %i" part2
    0 // return an integer exit code