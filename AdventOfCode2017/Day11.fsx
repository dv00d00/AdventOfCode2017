let step (x,y,z) dir  = 
    match dir with
    | "n"  -> (x, y, z+1)
    | "s"  -> (x, y, z-1)
    | "sw" -> (x, y+1, z)
    | "ne" -> (x, y-1, z)    
    | "se" -> (x+1, y, z)
    | "nw" -> (x-1, y, z)   

let length (x,y,z) = (abs x) + (abs y) + (abs z)

let subtract (x, y, z) d = (x - d, y - d, z - d)

let minimize ((x, y, z) as vector) = 
  let [_;mid;_] = [x; y; z] |> List.sort
  in length (subtract vector mid)  

let solution1 (input:string) = 
    input.Split(',') 
    |> Array.fold step (0,0,0)
    |> minimize

let question = __SOURCE_DIRECTORY__ + ".\Day11.txt" |> System.IO.File.ReadAllText
solution1 question

let solution2 (input:string) = 
    input.Split(',') 
    |> Array.scan step (0,0,0)
    |> Array.map minimize
    |> Array.max

solution2 question