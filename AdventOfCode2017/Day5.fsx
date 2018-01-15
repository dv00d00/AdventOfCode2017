module Take1 = 

    open System.IO

    let sample = [|"0";"3";"0";"1";"-3"|]
    let jumps = sample |> Array.map int

    let jump1 (store: int[]) ptr = 
        let offset = store.[ptr]
        store.[ptr] <- offset + 1
        ptr + offset

    let jump2 (store: int[]) ptr = 
        let offset = store.[ptr]
        if offset >= 3 then
            store.[ptr] <- offset - 1
        else 
            store.[ptr] <- offset + 1
        ptr + offset

    let answer (jumps:int[]) jumper = 
        let jumps = jumps |> Array.copy
        let mutable counter = 0
        let mutable int_ptr = 0

        while int_ptr >= 0 && int_ptr < jumps.Length do
            counter <- counter + 1
            int_ptr <- jumper jumps int_ptr

        counter

    // answer jumps

    let yo = 
        __SOURCE_DIRECTORY__ + ".\Day5.txt" 
        |> File.ReadAllLines
        |> Array.map int

    answer jumps jump1
    answer jumps jump2
    answer (yo) jump1
    answer (yo) jump2

module Take2 =

    open System.IO

    let mut1 offset = offset + 1
    let mut2 offset = if offset >= 3 then offset - 1 else offset + 1
        
    let jump mutator (store: int[]) ptr  = 
        let offset = store.[ptr]
        store.[ptr] <- mutator offset
        offset

    let answer (jumps:int[]) jumper = 
        let jumps = jumps |> Array.copy
        let counter = ref 0
        let mutable int_ptr = 0

        while int_ptr >= 0 && int_ptr < jumps.Length do
            incr counter
            int_ptr <- jumper jumps int_ptr

        counter   

    let question = 
        __SOURCE_DIRECTORY__ + ".\Day5.txt" 
        |> File.ReadAllLines
        |> Array.map int

    let sample = [|"0";"3";"0";"1";"-3"|] |> Array.map int

    answer sample (jump mut1)
    answer sample (jump mut2)

    answer question (jump mut1)
    answer question (jump mut2)