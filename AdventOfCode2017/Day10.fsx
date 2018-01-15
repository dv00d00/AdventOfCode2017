module Day10 =
    open System

    let reverse nums pos n = 
        let c = Array.copy nums
        for i in [0..n-1] do
            nums.[(pos + i) % 256] <- c.[(pos + n - i - 1) % 256] 

    let hash nRounds input =
        let nums = [|0..255|]
        let mutable pos = 0
        let mutable skipsize = 0

        for _ in [1..nRounds] do
            for n in input do
                reverse nums pos n
                pos <- pos + n + skipsize
                skipsize <- skipsize + 1
        nums

    let solution1 (input: string) =
        input.Split(',')
        |> Array.map int
        |> hash 1
        |> Array.take 2
        |> Array.reduce (*)

    let knotHash (input: string) =
        input
        |> Seq.map (fun c -> Convert.ToByte(c))
        |> fun a -> Seq.append a [|17uy; 31uy; 73uy; 47uy; 23uy|]
        |> Seq.map int
        |> hash 64 
        |> Array.chunkBySize 16
        |> Array.map (Array.reduce (^^^))
        |> Array.fold (fun str digit -> str + sprintf "%02x" digit) ""

    solution1 "120,93,0,90,5,80,129,74,1,165,204,255,254,2,50,113"
    knotHash "120,93,0,90,5,80,129,74,1,165,204,255,254,2,50,113"


module Day14 = 
    open Day10
    open System

    let input = "oundnydw"

    let data = [|        
        for i = 0 to 127 do
            yield input + "-" + i.ToString()
    |]

    let hashed = data |> Array.map knotHash 
    
    let inline (++) a b = b |> Array.append a

    let countBinaryOnes : char -> int =         
        let table1 = [| '0' .. '9' |] ++ [| 'a' .. 'f' |]
        let table2 = [|0..15|] |> Array.map (fun n -> Convert.ToString(n, 2) |> Seq.fold (fun state it -> if it = '1' then state+1 else state) 0 ) 
        let table3 = Map.ofArray ( Array.zip table1 table2)
        fun hexn -> table3.[hexn]


       
    let answer1 = hashed |> Array.sumBy (fun row -> row |> Seq.sumBy countBinaryOnes)

    

    let isAdjecentPoints (x1,y1) (x2,y2) = 
        let dx,dy = abs (x1-x2), abs (y1-y2)
        match dx,dy with
        | (1,0) | (0,1) -> true
        | _ -> false

    let isAdjecentRegions set1 set2 = 
        set2 |> Set.exists (fun p1 -> set1 |> Set.exists ( fun p2 -> isAdjecentPoints p1 p2))

    type Point = int * int
    type Region = Set<Point>

    let toBin : char -> char[] =
        let table1 = [| '0' .. '9' |] ++ [| 'a' .. 'f' |]
        let table2 = [|0..15|] |> Array.map (fun n -> Convert.ToString(n, 2).PadLeft(4, '0').ToCharArray() ) 
        let table3 = Map.ofArray ( Array.zip table1 table2)
        fun hexn -> table3.[hexn]

    let binData = 
        hashed 
        |> Array.map (fun row -> row.ToCharArray() |> Array.collect toBin )

    let allPoints = 
        [|            
            for y = 0 to 127 do
            for x = 0 to 127 do
                yield (x,y)            
        |]

    let step1 = 
        allPoints |> Array.fold (fun sets ((x,y) as p) -> 
            let isOne = binData.[x].[y] = '1'
            if isOne then
                let set = sets |> List.tryFind (fun set -> set |> Set.exists (fun p' -> isAdjecentPoints p' p))
                match set with
                | None -> (Set.singleton p) :: sets 
                | Some set -> 
                    let set' = set |> Set.add p
                    sets |> List.map (fun set'' -> if set'' = set then set' else set'')
            else
                sets
            ) []

    step1 |> List.length

    let fold step1 = 
        step1 |> List.fold (fun state set1 -> 
                    let adj = state |> List.tryFind (fun set2 -> isAdjecentRegions set1 set2)
                    match adj with
                    | Some adj -> 
                        let filtered = state |> List.filter (fun set2 -> set2 <> adj) 
                        (Set.union adj set1) :: filtered
                    | None -> set1 :: state
            ) []

    (step1 |> fold |> fold |> fold |> fold |> fold |> fold |> fold |> fold |> fold |> fold).Length