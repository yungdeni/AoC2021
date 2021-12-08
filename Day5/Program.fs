open System.IO
let trim (line:string) = 
    line.Trim()
let split (line:string) = 
    line.Split("->")
    |> Array.map trim

let split2intTup(line:string) = 
    let res = line.Split(",")
    let a = int res.[0]
    let b = int res.[1]
    (a,b)

let getSign num = 
    match num with
    |num when num < 0 -> -1
    |num when num > 1 -> 1
    |_ -> 0
let filterOutDiagonals (entry:(int*int)[]) = 
    let x1, y1 = entry.[0]
    let x2, y2 = entry.[1]
    match x1,x2,y1,y2 with
    |x1,x2,y1,y2 when x1 = x2 -> true
    |x1,x2,y1,y2 when y1 = y2 -> true
    |_ -> false
let filterOutNot45degree (entry:(int*int)[]) = 
    let x1, y1 = entry.[0]
    let x2, y2 = entry.[1]
    match x1,x2,y1,y2 with
    |x1,x2,y1,y2 when (x1 - x2) = (y1 + y2) -> true
    |x1,x2,y1,y2 when abs (x1 - x2) = abs (y1 - y2) -> true
    |x1,x2,y1,y2 when x1 = x2 -> true
    |x1,x2,y1,y2 when y1 = y2 -> true
    |_ -> false
let getPoints (entry:(int*int)[]) = 
     let x1, y1 = entry.[0]
     let x2, y2 = entry.[1]
     let dx = x2 - x1
     let dy = y2 - y1
     let X = 
        match getSign dx with
        |0 -> [for i in 0 .. abs dy -> x1]
        |1 -> [x1 ..x2]
        |(-1) -> [x1..(-1)..x2]
        |_ -> []
     let Y = 
        match getSign dy with
        |0 -> [for i in 0 .. abs dx ->y1]
        |1 -> [y1..y2]
        |(-1) -> [y1..(-1)..y2]
        |_ -> []
     List.zip X Y
//Mutable yikes..
let solveOneLine (emptyArray:int[,]) (points:(int * int) list) = 
    for a, b in points do
        emptyArray.[a,b] <- emptyArray.[a,b] + 1
    emptyArray
let plotter (emptyArray:int[,]) (points:(int * int) list[]) = 
    let rec inner (pointArray:int[,]) (points:(int * int) list[]) = 
        //let pointArray = solveOneLine pointArray points.[0]
        match points with
        |points when points.Length = 1 -> solveOneLine pointArray points.[0]
        |_ -> inner (solveOneLine pointArray points.[0]) points.[1..]
    inner emptyArray points
let Array2Dflatter (arr:int[,]) =
    let ret = [for i in 0..arr.GetLength(0)-1 -> arr.[*,i]]
    ret

let overlaps (inp:int[] list) = 
    let rec inner (inp:int[] list) acc = 
        match inp with
        |[] -> acc
        |_ -> inner inp.Tail (acc + (Array.filter (fun elm -> elm > 1) inp.Head).Length)
    inner inp 0
[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
        |> Array.map split
        |> Array.map (Array.map split2intTup)

    let part1plot = 
        input
        |> Array.filter filterOutDiagonals
        |> Array.map  getPoints
        |> plotter (Array2D.create 1000 1000 0) 

    let part1 = 
        Array2Dflatter part1plot
        |> overlaps
    let part2points = 
        input
        |> Array.filter filterOutNot45degree
        |> Array.map  getPoints
    let part2plot = 
        input
        |> Array.filter filterOutNot45degree
        |> Array.map  getPoints
        |> plotter (Array2D.create 1000 1000 0) 

    let part2 = 
         Array2Dflatter part2plot
         |> overlaps
    printfn "Part 1: %i" part1
    printfn "Part 2: %i" part2
    0 // return an integer exit code