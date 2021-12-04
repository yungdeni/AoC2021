open System.IO

let bitwisex a b = 
    a ^^^ b
let binaryToInt stringbinary = 
    let bin = "0b" + string stringbinary
    uint bin
let binaryToIntArray (stringbinary:string) = 
    [for i in 0 .. stringbinary.Length-1 -> int (binaryToInt stringbinary.[i])]
let countBits listA listB =
    List.map2 (+) listA listB
    |> List.map int
let returnMostCommon counts (len:int)  = 
    counts
    |> List.map float
    |> List.map (fun i -> if i >= float len/2.0 then 1 else 0 ) 
let invertBits str = 
    match str with
    |"1" -> "0"
    |"0" -> "1"
    |_ -> "Error"
let filter (inp:string[]) (mask:string list) (ind:int) =  
    inp
    |>Array.filter (fun i ->string i.[ind] =  mask.[0])
let bitcountHelper input ind  =
    let part = 
        input
        |> Array.map binaryToIntArray
        |> Array.reduce countBits
    returnMostCommon part input.Length 
    |> List.map (fun i -> i.ToString())
    |> List.skip ind


let OxyFilterOuter (inp:string[]) (mask:string list) =
    let rec Oxyfilter (inp:string[]) (mask:string list) (ind:int) = 
        let iterMask = 
            bitcountHelper inp (ind + 1) 
        match inp with
        |inp when inp.Length = 1 -> inp
        |_ ->  Oxyfilter (filter inp iterMask (ind+1)) iterMask.Tail (ind+1)
    Oxyfilter inp mask -1
let CarbFilterOuter (inp:string[]) (mask:string list) =
    let rec Carbfilter (inp:string[]) (mask:string list) (ind:int) = 
        let iterMask = 
            bitcountHelper inp (ind + 1) 
            |> List.map invertBits
            
        match inp with
        |inp when inp.Length = 1 -> inp
        |_ ->  Carbfilter (filter inp iterMask (ind+1)) iterMask.Tail (ind+1)
    Carbfilter inp mask -1

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")


    let part1list = 
        bitcountHelper input 0 

    let gamma =
        part1list
        |> List.fold (+) ""
        |> binaryToInt
    let epsilon = 
        part1list 
        |> List.map invertBits
        |> List.fold (+) ""
        |> binaryToInt
    let part2Oxy = 
        OxyFilterOuter input part1list
        |> Array.toList
        |>List.fold (+) ""
        |>binaryToInt
    let part2Carb = 
        CarbFilterOuter input part1list
        |> Array.toList
        |>List.fold (+) ""
        |>binaryToInt

    let part1 = gamma * epsilon
    let part2 = part2Oxy * part2Carb
    let test = uint "1"
    printfn "Part1 %i" part1
    printfn "Part2 %i" part2
    0 // return an integer exit code