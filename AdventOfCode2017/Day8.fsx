type Register = string
type Value = int
type Command = Inc of Register*Value | Dec of Register * Value
type Comparison = Eq | GeEq | LeEq | Ge | Le | NeEq
type Condition = Condition of Comparison * Register * Value
type Instruction = Instruction of Command * Condition
type State = Map<Register, Value>

let parse (raw:string) = 
    let parts = raw.Split(' ')

    let command = 
        let register = parts.[0]
        let comp = parts.[1]
        let value = parts.[2] |> int

        if comp = "inc" then 
            Inc(register, value)
        else
            Dec(register, value) 

    let condition = 
        let register = parts.[4]
        let operator = parts.[5]
        let value = parts.[6] |> int

        let comp = 
            match operator with            
            | ">" -> Ge    
            | "<" -> Le
            | ">=" -> GeEq 
            | "<=" -> LeEq
            | "!=" -> NeEq
            | "==" -> Eq

        Condition (comp, register, value)

    Instruction (command, condition)

let regVal (state:State) (reg:Register) = state |> Map.tryFind reg |> Option.defaultValue 0

let isTrue (Condition (comp, reg, value)) (state:State) = 

    let regVal = regVal state reg 

    match comp with
    | Eq   -> regVal =  value
    | Ge   -> regVal >  value
    | Le   -> regVal <  value
    | GeEq -> regVal >= value
    | LeEq -> regVal <= value
    | NeEq -> regVal <> value

let update (state:State) (register:Register) (newValue:Value) = 
    if state |> Map.containsKey register then
        state |> Map.remove register |> Map.add register newValue
    else
        state |> Map.add register newValue

let eval (state:State) (Instruction (command, condition)) : State = 
    
    let regVal = regVal state
    let update = update state
    let isTrue condition = isTrue condition state

    if isTrue condition then
        match command with
        | Inc (reg, value) -> 
            let nv = (regVal reg) + value
            update reg nv
        | Dec (reg, value) ->
            let nv = (regVal reg) - value
            update reg nv
    else 
        state

let sample = 
    [|       
        "b inc 5 if a > 1"
        "a inc 1 if b < 5"
        "c dec -10 if a >= 1"
        "c inc -20 if c == 10"
    |]

let solution sample = 
    sample 
    |> Array.map parse 
    |> Array.fold eval Map.empty 
    |> Map.toArray 
    |> Array.maxBy snd
    
let solution2 sample = 
    sample 
    |> Seq.map parse 
    |> Seq.scan eval Map.empty 
    |> Seq.map ( fun state ->
        if state = Map.empty 
        then None
        else 
            state 
            |> Map.toArray 
            |> Array.maxBy snd 
            |> Some
        )
    |> Seq.choose id
    |> Seq.maxBy snd
    
let input = 
     __SOURCE_DIRECTORY__ + ".\Day8.txt"
     |> System.IO.File.ReadAllLines

solution input
solution2 input