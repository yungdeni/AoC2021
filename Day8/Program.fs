open System.IO
open System


let splitLine  (outPut:bool) (str:string)= 
    let res = str.Split('|')
    let a = res.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let b = res.[1].Split(" ",StringSplitOptions.RemoveEmptyEntries)
    if outPut then b else a

let splitLineBoth  (str:string)= 
    let res = str.Split('|')
    let a = res.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let b = res.[1].Split(" ",StringSplitOptions.RemoveEmptyEntries)
    (a,b)

let part1filter (str:string) = 
    match str.Length with
    |2 -> true
    |3 -> true
    |4 -> true
    |7 -> true
    |_ -> false

let sortByLength (input:string[]) = 
    Array.sortBy (fun (x : string) -> x.Length) input

let concatInt (inp:int[]) = 
    inp |> Array.map string |> Array.reduce (+) |> int

let getdiff (a:string) (b:string) = 
    let set1 = Set.ofArray (a.ToCharArray())
    let set2 = Set.ofArray (b.ToCharArray())
    Set.toList(Set.difference set1 set2)

let containsInSet (a:string) (b:string) = 
    let set1 = Set.ofArray (a.ToCharArray())
    let set2 = Set.ofArray (b.ToCharArray())
    (Set.isSubset set1 set2)

let getSet (str:string) = 
    Set.ofArray (str.ToCharArray())
let getValuesWithLength len = 
    (fun (v:string) -> if (v.Length = len) then Some v else None)
let getKeys (input:string[]) = 
    let len5 = Array.choose (getValuesWithLength 5) input
    let len6 = Array.choose (getValuesWithLength 6) input
    let one, four, seven, eight = input.[0], input.[2], input.[1], input.[9]
    let three = Array.find (containsInSet one) len5
    let nine = Array.find (containsInSet three) len6
    let len5Rem = Array.filter (fun v -> v <> three) len5
    let len6Rem = Array.filter (fun v -> v <> nine) len6
    let zero = Array.find (containsInSet one) len6Rem
    let six = Array.find (fun v -> v <> zero) len6Rem
    let bd = String.concat "" <| List.map string (getdiff four one) 
    let five = Array.find (containsInSet (bd.ToString())) len5Rem
    let two = Array.find (fun v -> v <> five) len5Rem
    Map.ofList [getSet zero,0; getSet one, 1;getSet two, 2 ; getSet three, 3;getSet four, 4;getSet five, 5; getSet six, 6; getSet seven,7 ; getSet eight, 8;getSet nine,9]

let part2Solver (line:(string[] * string[])) = 
    let inp,output = line
    let keys = getKeys (sortByLength inp)
    let outputSets = Array.map getSet output
    outputSets |> Array.map (fun v -> Map.find v keys)


[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    let uniqueDigits = 
        input
        |> Array.collect (splitLine true)
        |> Array.filter part1filter
    let part1 = uniqueDigits.Length
    let part2 = 
        input
        |> Array.map splitLineBoth
        |> Array.map part2Solver
        |> Array.map concatInt
        |> Array.sum
    printfn "Part 1: %i" part1
    printfn "Part 2: %i" part2
    0 // return an integer exit code