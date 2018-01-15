open System

type Id = int
type Row = Id * Id list
type Group = Id Set

let sample = [|
    "0 <-> 2"
    "1 <-> 1"
    "2 <-> 0, 3, 4"
    "3 <-> 2, 4"
    "4 <-> 2, 3, 6"
    "5 <-> 6"
    "6 <-> 4, 5"
    |]

let parse (row:string) : Row = 
    let [|id; links|] = row.Split( [| "<->" |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim())
    let links = links.Split([| ", " |], StringSplitOptions.RemoveEmptyEntries) 
    id |> int, links |> Array.map int |> List.ofArray

let intersects xs ys = 
    xs |> Set.exists (fun x -> ys |> Set.contains x)
    
let composeGroups input = 
    let rows = input |> Array.map parse

    let folder (groups: Group list) (element:Row) = 
        
        let candidate = 
            let id, links = element
            set (id::links)

        let intersecting = groups |> List.filter ( fun group -> group |> intersects candidate)
        
        if intersecting <> [] then
            let unified = Set.unionMany (candidate::intersecting)
            unified :: (groups |> List.filter (fun it -> not ( intersecting |> List.contains it)))
        else
            candidate :: groups

    rows |> Array.fold folder []

composeGroups sample 
    |> List.find (fun set -> set.Contains 0) 
    |> Seq.length


let input = __SOURCE_DIRECTORY__ + ".\Day12.txt" |> System.IO.File.ReadAllLines


let answer1 = 
    composeGroups input 
    |> List.find (fun set -> set.Contains 0) 
    |> Seq.length

let answer2 = 
    composeGroups input     
    |> Seq.length