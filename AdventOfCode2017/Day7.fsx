open System.Collections.Generic

type Row = { Name : string; Weight: int; Children : string[] }

let parse (row:string) = 
    let parts = row.Split(' ')
    let name = parts.[0]
    let weight = parts.[1].Replace("(", "").Replace(")", "") |> int
    let hasChildren = parts.Length > 2
    if hasChildren then
        let children = parts.[3..parts.Length-1] |> Array.map(fun it -> it.Replace(",", "").Trim())
        { Name = name; Weight = weight; Children = children }
    else
        { Name = name; Weight = weight; Children = Array.empty }

let input = 
     __SOURCE_DIRECTORY__ + ".\Day7.txt"
     |> System.IO.File.ReadAllLines
 
let test_input = 
    [|
        "pbga (66)"
        "xhth (57)"
        "ebii (61)"
        "havc (66)"
        "ktlj (57)"
        "fwft (72) -> ktlj, cntj, xhth"
        "qoyq (66)"
        "padx (45) -> pbga, havc, qoyq"
        "tknk (41) -> ugml, padx, fwft"
        "jptl (61)"
        "ugml (68) -> gyxo, ebii, jptl"
        "gyxo (61)"
        "cntj (57)"
    |]

type Node = { Name : string; Weight: int; Children : Node[] }

let rec depth (node:Node) = 1 + (node.Children |> Array.sumBy depth)

let solution1 input = 
    let rows = input |> Array.map parse
    let map = rows |> Array.map (fun row -> (row.Name, row)) |> Map.ofArray

    let rec buildNode (map:Map<string, Row>) (name:string) : Node = 
        let row = map |> Map.find name
        {
            Name = row.Name
            Weight = row.Weight
            Children = row.Children |> Array.map (buildNode map)
        }

    let trees = rows |> Array.map (fun row -> buildNode map row.Name)

    trees |> Array.maxBy depth
    
let answer1 = solution1 input
answer1.Name

let rec weight node = node.Weight + (node.Children |> Array.sumBy (weight))

let balanced node = 
    node.Children |> Array.map (weight) |> Array.distinct |> Array.length = 1

let rec solution2 (papa:Node) = 
    
    let bad_child = papa.Children |> Array.maxBy (weight)
    if balanced bad_child then
        let weights = papa.Children |> Array.map (weight)
        bad_child.Weight - ((weights |> Array.max) - (weights |> Array.min))
    else 
        solution2 bad_child

solution2 answer1