let banks = [0;5;10;0;11;14;13;4;11;8;8;7;1;4;12;11]

let step input = 
    let input = input |> Array.ofList
    let maxv = Array.max input
    let maxi = input |> Array.findIndex ((=)maxv)
        
    input.[maxi] <- 0
    let mutable i = maxi + 1
    for _ in 1..maxv do        
        let ptr = i % input.Length
        input.[ptr] <- input.[ptr] + 1
        i <- i + 1

    input |> List.ofArray

let solution1 input = 
    let steps = 
        Seq.unfold (
            fun state -> 
                let head = List.head state
                let next = step head
                if (List.exists ((=)next) state) 
                    then None 
                    else Some((), next::state)
        ) [input]
        |> Seq.length
    steps + 1

solution1 banks

let solution2 input = 
    Seq.unfold (
        fun state -> 
            let head = List.head state
            let next = step head
            Some(next, next::state)
    ) [input]
    |> Seq.take (7864*2)
    |> Array.ofSeq

let twoCycles = solution2 banks
let hashes = twoCycles |> Array.map (fun it -> it.GetHashCode())
let candidate = hashes.[7863]

hashes 
|> Array.indexed 
|> Array.filter (fun (i,x) -> x = candidate) 
|> Array.map fst
|> Array.pairwise
|> Array.map (fun (a,b) -> b - a)