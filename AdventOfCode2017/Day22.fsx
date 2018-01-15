open System

type Carier = { Position: Position; Direction: AbsoluteDirection }
and Position = int * int
and AbsoluteDirection = Up | Left | Down | Right
type TurnDirection = ToLeft | ToRight

type Grid = Map<Position, NodeStatus>
and NodeStatus = Infected | Clean

type Stats = { NodeBecomeInfected: int}

let turn (turnTo:TurnDirection) (dir:AbsoluteDirection)  = 
    if turnTo = ToLeft then
        match dir with
        | Up -> Left 
        | Left -> Down 
        | Down -> Right
        | Right-> Up 
    else
        match dir with
        | Up -> Right
        | Left -> Up 
        | Down -> Left 
        | Right -> Down 

let move ((x,y):Position) (dir:AbsoluteDirection) : Position = 
    match dir with
    | Up -> (x, y-1)
    | Down -> (x, y+1)
    | Left -> (x-1, y)
    | Right -> (x+1, y)

let step ((carier: Carier), (grid:Grid), (stats:Stats)) : (Carier*Grid*Stats) = 

    let currentNodeStatus = 
        grid 
        |> Map.tryFind carier.Position
        |> Option.defaultValue Clean

    let newCurrentNodeStatus = 
        if currentNodeStatus = Infected then Clean else Infected

    let nextStats = 
        if newCurrentNodeStatus = Infected then
            { NodeBecomeInfected = stats.NodeBecomeInfected + 1 }
        else    
            stats

    let nextGrid =         
        grid |> Map.add (carier.Position) newCurrentNodeStatus

    let nextCarier =         
        let newCarierDirection = 
            if currentNodeStatus = Infected then
                carier.Direction |> turn ToRight
            else 
                carier.Direction |> turn ToLeft

        let newCarierPosition = move carier.Position newCarierDirection

        { Position = newCarierPosition; Direction = newCarierDirection }

    nextCarier, nextGrid, nextStats

let input = 
    """.#...#.#.##..##....##.#.#
###.###..##...##.##....##
....#.###..#...#####..#.#
.##.######..###.##..#...#
#..#..#..##..###...#..###
..####...#.##.#.#.##.####
#......#..####..###..###.
#####.##.#.#.##.###.#.#.#
.#.###....###....##....##
.......########.#.#...#..
...###.####.##..###.##..#
#.#.###.####.###.###.###.
.######...###.....#......
....##.###..#.#.###...##.
#.###..###.#.#.##.#.##.##
#.#.#..###...###.###.....
##..##.##...##.##..##.#.#
.....##......##..#.##...#
..##.#.###.#...#####.#.##
....##..#.#.#.#..###.#..#
###..##.##....##.#....##.
#..####...####.#.##..#.##
####.###...####..##.#.#.#
#.#.#.###.....###.##.###.
.#...##.#.##..###.#.###.."""

let testInput = 
    """..#
#..
..."""

let parseInput (input:string) : Carier * Grid * Stats = 
    let lines = 
        input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> 
            line.ToCharArray() 
            |> Array.filter (fun c -> c = '.' || c = '#')
            |> Array.map ( fun c -> 
                    if c = '.' then Clean 
                    elif c = '#' then Infected
                    else failwith "wtf"
                ))

    let width = lines.[0].Length
    let height = lines.Length

    let start = 
        let y = height / 2
        let x = width / 2
        (x,y)

    let grid = 
        seq { 
            for x = 0 to width-1 do  
            for y = 0 to height-1 do
            yield ((x,y), lines.[y].[x]) }
        |> Map.ofSeq

    ({ Position = start; Direction = Up }, grid, { NodeBecomeInfected = 0 })
        
let answer1 parsedInput = 
    Seq.unfold (fun state -> let next = step state in Some (next,next)) parsedInput
    |> Seq.skip (10000000-1)
    |> Seq.head
    |> fun (_,_,s) -> s.NodeBecomeInfected

parseInput testInput
answer1 (parseInput input)
