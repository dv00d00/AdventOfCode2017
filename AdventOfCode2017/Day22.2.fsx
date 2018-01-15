open System

type Carier = { Position: Position; Direction: AbsoluteDirection }
and Position = int * int
and AbsoluteDirection = Up | Left | Down | Right
type TurnDirection = ToLeft | ToRight | Backwards

type Grid = Map<Position, NodeStatus>
and NodeStatus = Infected | Clean | Weakened | Flagged

type Stats = { NodeBecomeInfected: int}

let turn (turnTo:TurnDirection) (dir:AbsoluteDirection)  = 
    match turnTo with
    | ToLeft ->
        match dir with
        | Up -> Left 
        | Left -> Down 
        | Down -> Right
        | Right-> Up 
    | ToRight ->
        match dir with
        | Up -> Right
        | Left -> Up 
        | Down -> Left 
        | Right -> Down 
    | Backwards -> 
        match dir with
        | Up -> Down
        | Left -> Right 
        | Down -> Up 
        | Right -> Left

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
        match currentNodeStatus with
        | Clean -> Weakened
        | Weakened -> Infected
        | Infected -> Flagged
        | Flagged -> Clean

    let nextStats = 
        if newCurrentNodeStatus = Infected then
            { NodeBecomeInfected = stats.NodeBecomeInfected + 1 }
        else    
            stats

    let nextGrid =         
        grid |> Map.add (carier.Position) newCurrentNodeStatus

    let nextCarier =         
        let newCarierDirection = 
            match currentNodeStatus with
            | Clean -> turn ToLeft (carier.Direction)
            | Weakened -> carier.Direction
            | Infected -> turn ToRight (carier.Direction)
            | Flagged -> turn Backwards (carier.Direction)

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
        
let answer2 parsedInput = 
    let mutable state = parsedInput

    for i = 1 to 10000000 do
        state <- step state

    let (_,_,a) = state
    a.NodeBecomeInfected

answer2 (parseInput input)
