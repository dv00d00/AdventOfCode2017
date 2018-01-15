open System

// ../.. => .#./.../###
// .../.../... => .###/.#.#/.###/##.#
module String = 
    let split (sep:string) (str:string)  : string[] = 
        str.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

type Matrix<'t> = 't[,]

let rotate (data:Matrix<_>) = 
    let L = data.GetUpperBound(0)
    data |> Array2D.mapi (fun x y _ -> data.[L-y, x])

let flipV (data:Matrix<_>) = 
    let L = data.GetUpperBound(0)
    data |> Array2D.mapi (fun x y _ -> data.[x, L- y])

let rotations (data:Matrix<_>) = 
    [|1..4|] |> Array.scan (fun it _ -> rotate it) data 

let updates m = 
    let (++) = Array.append
    (m |> rotations) ++ (m |> flipV |> rotations) 
    |> Array.distinct

let inline toMatrix (it:'t[][]) =
    let L = it.Length
    Array2D.init L L (fun i j -> it.[i].[j])

let parse line = 
    let [|input; output|] = 
        line 
        |> String.split " => " 
        |> Array.map (String.split "/")        
        |> Array.map (Array.map ( Array.ofSeq ))
        |> Array.map (toMatrix)

    let result = updates input
    (result, output)

let rules = __SOURCE_DIRECTORY__ + "\Day21.txt" |> System.IO.File.ReadAllLines |> Array.map parse
//let rules = [|"../.# => ##./#../...";".#./..#/### => #..#/..../..../#..#"|] |> Array.map parse

let transform (state : _ Matrix )=
    let (_, out) = rules |> Array.find (fun (ins, out) -> ins |> Array.exists ((=)state) )
    out 

let split len (state : _ Matrix) = 
    let L = state.GetUpperBound 0 + 1

    [|
        for i in [0..(L/len-1)] do 
        yield
            [| 
                for j in [0..(L/len-1)] do
                yield Array2D.init len len (fun x y -> state.[len*i+x,len*j+y]) 
            |]
    |] 
    
    |> toMatrix

let merge (matrices: _ Matrix Matrix) : _ Matrix = 

    let count = matrices.GetUpperBound 0 + 1 
    let segmenSize = matrices.[0,0].GetUpperBound 0 + 1

    let L = count * segmenSize

    Array2D.init L L (fun x y -> 
            let i,j = x / segmenSize, y / segmenSize
            let a,b = x % segmenSize, y % segmenSize
            matrices.[i,j].[a,b]
        )

let step (state : _ Matrix) = 
    let L = state.GetUpperBound(0) + 1
    let side = if L % 2 = 0 then 2 else 3    
    state |> split side |> Array2D.map transform |> merge

let seed = 
    [|
        [|'.';'#';'.'|]
        [|'.';'.';'#'|]
        [|'#';'#';'#'|]
    |] 
    |> toMatrix

    |> step |> step |> step
    |> step |> step 
    
    |> step
    |> step |> step |> step
    |> step |> step |> step
    |> step |> step |> step
    |> step |> step |> step
        
    |> Seq.cast
    |> Seq.map (fun it -> if it = '#' then 1 else 0)
    |> Seq.sum