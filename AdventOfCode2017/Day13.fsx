type Time = int
type Height = int
type Severity = int
type Row = Time * Height
type Input = Row[]


let hitsRoof height maxTime = 
    let roundtripDistance = (height - 1) * 2
    
    Array.init (maxTime+1) id
    |> Array.filter (fun time -> time % roundtripDistance = 0 )

let inline hits (height:Height) (time:Time) = 
    let roundtripDistance = (height - 1) * 2
    time % roundtripDistance = 0


//hitsRoof 4 7 |> Array.ofSeq    

let input: Input = [|
    (0, 3)
    (1, 2)
    (4, 4)
    (6, 4)
|]

let solve (input:Input) : Severity = 
    let totalTravelTime = 
        input 
        |> Array.map fst 
        |> Array.max
 
    input 
        |> Array.map (fun (time, height) -> time, height, hitsRoof height totalTravelTime )
        |> Array.filter (fun (time, _, hits) -> hits |> Array.contains time)
        |> Array.map(fun (time, height, _) -> time * height)
        |> Array.sum



let inline willBeCaught ((delay:Time),(input:Input)) : bool =         
    Array.exists ( fun (time, height) -> hits height (time + delay)) input
   
let solve2 input = 
    let time = Seq.initInfinite id

    time
    |> Seq.filter (fun delay -> not <| willBeCaught(delay,input))
    |> Seq.head


let solve3 input : int =     
    let mutable delay = 0;
    while willBeCaught (delay,input) do
        delay <- delay + 1
    delay
    

let actual_input = [|
    (0, 4); (1, 2); (2, 3); (4, 5); (6, 6); (8, 4); (10, 8); (12, 6); (14, 6); (16, 8); (18, 8); (20, 6); (22, 8); (24, 9); (26, 8);
    (28, 8); (30, 12); (32, 12); (34, 10); (36, 12); (38, 12); (40, 10); (42, 12); (44, 12); (46, 12); (48, 12); (50, 12); (52, 14);
    (54, 14); (56, 12); (58, 14); (60, 14); (62, 14); (64, 17); (66, 14); (70, 14); (72, 14); (74, 14); (76, 14); (78, 18); (82, 14);
    (88, 18); (90, 14); 
|]

solve input
solve actual_input
solve2 input
solve3 actual_input