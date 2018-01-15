// Learn more about F# at http://fsharp.org

open System

type Grid = char [] []
type Position = int * int
type Direction = Up | Down | Left | Right

let direction (x1,y1) (x2,y2) = 
    match (x2-x1, y2-y1) with
    | 1, 0 -> Down
    | -1, 0 ->  Up
    | 0, 1 -> Right
    | 0, -1 -> Left


let valid (row,col) (grid:Grid) = 
    let height = grid.Length
    let width = grid.[0].Length
    row >= 0 && row < height && col >=0 && col < width

let tryGetNextStep (grid:Grid) ((row, col) as current) curdir = 

    let candidates = [ (row+1,col); (row-1,col); (row,col+1); (row, col-1) ]
    candidates 
        |> List.filter (fun p -> valid p grid)
        |> List.filter (fun (row,col) -> grid.[row].[col] <> ' ')
        |> List.filter (fun pnext -> direction pnext current <> curdir)
        |> List.tryHead


let input = 
 """     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-X  +--+ """

let getGrid (input:string) = 
    input.Split('\n')
    |> Array.map (fun x -> x.ToCharArray())

let findEntrance (grid:Grid) = 
    let col = grid.[0] |> Array.findIndex ((=) '|')
    0, col



type ParserState = {
    Grid: Grid;
    CurrentDirection: Direction
    Position: Position
    PreviousPosition: Position
    Letters: char list
}

let fresh grid start = { Grid = grid; CurrentDirection = Down; Position = start; PreviousPosition = (0,0); Letters = [] }

let next state = 

    let { Grid = grid;
          CurrentDirection = curdir; 
          Position = ((row,col) as currentPosition); 
          PreviousPosition = prevpos; 
          Letters = acc } = state

    let value = grid.[row].[col]        

    let candidate = tryGetNextStep grid currentPosition curdir
    
    let acc = 
        if System.Char.IsLetter value 
            then value :: acc 
            else acc

    match candidate with
    | Some nextPosition -> 
        let nextDirection = direction currentPosition nextPosition
        let state = { state with 
                        PreviousPosition = currentPosition;
                        Position = nextPosition;
                        Letters = acc;
                        CurrentDirection = nextDirection }

        Some( state.Letters, state)
    | _ ->  None

let grid = (getGrid input)
let start = fresh grid (findEntrance grid)

let testAnswer = 
    start
        |> Seq.unfold next 
        |> Seq.last 
        
        |> List.rev 
        |> Array.ofList 
        |> System.String


//let grid2 = (getGrid (__SOURCE_DIRECTORY__ + "\Day19.txt" |> System.IO.File.ReadAllText ))
//let start2 = fresh grid2 (findEntrance grid2)

//let answer = 
//    start2
//        |> Seq.unfold next 
//        |> Seq.last 
//        |> fun s -> s.Letters 
//        |> List.rev 
//        |> Array.ofList 
//        |> System.String