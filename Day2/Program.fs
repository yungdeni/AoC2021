open System.IO
type Position = int * int
type PosAim = int * int * int
type Direction = Forward | Up | Down | Error

let move (instr:Direction*int) (position:Position): Position =
    let x, y = position
    let dir,step = instr
    match dir with
    |Forward -> x + step, y
    |Down -> x, y + step
    |Up -> x, y - step
    |Error -> x, y
let moveWithAim (instr:Direction*int) (position:PosAim) : PosAim =
    let x,y,aim = position
    let dir, step = instr
    match dir with
    |Forward -> x + step, y + aim * step, aim
    |Down -> x,y, aim + step
    |Up -> x,y, aim - step
    |Error -> x,y,aim
let stringSplit (line:string) = 
    line.Split(' ')
let parseStrings (inp:string[]) :(Direction*int)=
    match inp.[0], (int inp.[1]) with
    |"forward", step -> (Forward,step)
    |"up", step -> (Up,step)
    |"down", step -> (Down,step)
    |_ -> (Error,0)
let rec part1solver moves pos =
    match moves with
    |[] -> pos
    |head::tail -> 
        let acc = move head pos
        part1solver tail acc
let rec part2solver moves posaim:PosAim = 
    match moves with
    |[] -> posaim
    |head::tail ->
        let acc = moveWithAim head posaim
        part2solver tail acc
[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
        |> Array.map (stringSplit >> parseStrings)
        |> Array.toList
    let part1 = 
        let x, y = part1solver input (0,0)
        x * y
    let part2 =
        let x,y,z = part2solver input (0,0,0)
        x * y
    printfn "Part 1 %i" part1
    printfn "Part 2 %i" part2
    0 // return an integer exit code