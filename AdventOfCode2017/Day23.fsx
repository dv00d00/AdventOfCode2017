type Register = char
type Value = int64
type Ref = Ref of Register | Val of Value

type Instruction = 
    | Set of reg:Register * ref:Ref
    | Sub of reg:Register * ref:Ref
    | Mul of reg:Register * ref:Ref
    | Jnz of test:Ref * offset:Ref

type Registers = Map<Register, Value>

let regVal register registers = 
    registers |> Map.tryFind register |> Option.defaultValue 0L

let valueOf ref registers = 
    match ref with
    | Ref register -> regVal register registers
    | Val v -> v

let update reg value registers = 
    registers |> Map.add reg value

let program = [|
    Set ('b', Val 99L)
    Set ('c', Ref 'b')
    Jnz (Ref 'a', Val 2L)
    Jnz (Val 1L, Val 5L)

    Mul ('b', Val 100L)
    Sub ('b', Val -100000L)
    Set ('c', Ref 'b')
    Sub ('c', Val -17000L)

    Set ('f', Val 1L)
    Set ('d', Val 2L)
    Set ('e', Val 2L)
    Set ('g', Ref 'd')
    Mul ('g', Ref 'e')
    Sub ('g', Ref 'b')
    Jnz (Ref 'g', Val 2L)
    Set ('f', Val 0L)
    Sub ('e', Val -1L)
    Set ('g', Ref 'e')
    Sub ('g', Ref 'b')
    Jnz (Ref 'g', Val -8L)
    Sub ('d', Val -1L)
    Set ('g', Ref 'd')
    Sub ('g', Ref 'b')
    Jnz (Ref 'g', Val -13L)
    Jnz (Ref 'f', Val 2L)
    Sub ('h', Val -1L)
    Set ('g', Ref 'b')
    Sub ('g', Ref 'c')
    Jnz (Ref 'g', Val 2L)
    Jnz (Val 1L, Val 3L)
    Sub ('b', Val -17L)
    Jnz (Val 1L, Val -23L)
|]

let run instruction registers = 
    let regVal r = regVal r registers
    let valueOf r = valueOf r registers
    let update r v = registers |> Map.add r v

    match instruction with 
    | Set (reg, ref) -> 
        let registers = update reg (valueOf ref)
        1, registers
    | Sub (reg, ref) -> 
        let registers = update reg (regVal reg - valueOf ref)
        1, registers
    | Mul (reg, ref) -> 
        let registers = update reg (regVal reg * valueOf ref)
        1, registers
    | Jnz (test, offset) -> 
        let test = valueOf test       

        if test <> 0L then
            let offset = valueOf offset
            (int offset, registers)
        else    
            (1, registers)

let hz program = 
    
    let computer = (program, 0, Map.empty)

    Seq.unfold (fun (program, intptr, regs) -> 

        if intptr < 0 || intptr > (Array.length program - 1) then
            None
        else
            let instr = Array.get program intptr
            let (offset, regs) = run instr regs          
            Some (regs, (program, intptr + offset, regs))
    ) computer
    |> Seq.last

hz program

// solution 2

let isPrime n =
    match n with
    | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
    | _ ->
        let maxDiv = int(System.Math.Sqrt(float n)) + 1
        let rec f d i = 
            if d > maxDiv then 
                true
            else
                if n % d = 0 then 
                    false
                else
                    f (d + i) (6 - i)     
        f 5 2

[109900 .. 17 ..  109900 + 17000] |> List.filter (not << isPrime) |> List.length