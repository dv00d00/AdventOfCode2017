open System.IO
open System

type Register = string
type Ref = Val of int64 | Reg of string

type Instruction =     
    | Set of Register * Ref
    | Add of Register * Ref
    | Mul of Register * Ref
    | Mod of Register * Ref
    | Jgz of Ref * Ref
    | Rcv of Register
    | Snd of Ref

let pline (line:string) = 
    let inline (|Ref|) value = 
        let (succ, integer) = Int64.TryParse value
        if succ then  Val integer
        else          Reg value

    let parts = line.Split(' ')
    match parts with
    | [| "snd"; Ref freq; |] -> Snd (freq)
    | [| "set"; a; Ref b |] -> Set (a, b)    
    | [| "add"; a; Ref b |] -> Add (a, b)
    | [| "mul"; a; Ref b |] -> Mul (a, b)
    | [| "mod"; a; Ref b |] -> Mod (a, b)
    | [| "rcv"; a; |] -> Rcv (a)
    | [| "jgz"; Ref test; Ref offset |] -> Jgz (test, offset)
    | x -> failwith ("can't parse " + x.ToString())

type Registers = Map<Register, int64>   
type SentValue = int64
type Inbox = SentValue list
type Offset = int64

type Effect<'t> = 
    | Receive of (SentValue -> 't)
    | Send of SentValue
    | Next of 't
    | Jump of Offset

let run instr (regs:Registers) : Effect<Registers> = 

    let inline regval key map = 
        map |> Map.tryFind key |> Option.defaultValue 0L

    let inline update key updater : Registers = 
        let current = regs |> regval key |> updater
        in regs |> Map.add key current

    let (|Deref|) = function
        | Val x -> x
        | Reg r -> regs |> regval r

    match instr with
    | Set (reg, (Deref v)) -> update reg (fun _ -> v) |> Next
    | Add (reg, (Deref v)) -> update reg (fun x -> x + v) |> Next
    | Mul (reg, (Deref v)) -> update reg (fun x -> x * v) |> Next
    | Mod (reg, (Deref v)) -> update reg (fun x -> x % v) |> Next

    | Jgz ((Deref test), (Deref offset)) ->
        if test > 0L
            then Jump (offset)
            else Next regs

    | Snd (Deref v) -> Send (v)
    | Rcv (reg) -> Receive (fun v -> update reg (fun _ -> v))

type Program = { code : Instruction[]; iptr: int64; inbox: Inbox; regs: Registers; id: int; sent: int }

let solve code = 

    let p1 = 
        let start = Map.empty |> Map.add "p" 0L
        in { code = code; iptr = 0L; inbox = []; regs = start; id = 0; sent = 0}
    
    let p2 = 
        let start = Map.empty |> Map.add "p" 1L
        in { code = code; iptr = 0L; inbox = []; regs = start; id = 1; sent = 0 }

    let inline (<|>) a (b:Lazy<_>) = 
        match a, b with
        | Some _, _ -> a
        | _ -> b.Force()

    let _run' (source:Program, target: Program) = 
        let instr = source.code.[int(source.iptr)]
        let result = run instr (source.regs)

        match result with
        | Next(r) -> Some ({ source with regs = r; iptr = source.iptr + 1L }, target)
        | Jump(o) -> Some ({ source with iptr = source.iptr + o }, target)                
        | Send(x) -> Some ({ source with iptr = source.iptr + 1L; sent = source.sent + 1 },
                           { target with inbox = target.inbox @ [x]; } )                
        | Receive(f) -> 
            match source.inbox with
            | x :: xs -> 
                let a = { source with regs = f(x); iptr = source.iptr + 1L; inbox = xs; }
                in Some (a, target)
            | _ -> None

    let go (p1, p2) = 
        let r1 = _run' (p1, p2)
        let r2 = lazy _run' (p2, p1)        
        (r1 <|> r2) |> Option.map (fun x -> x, x)

    Seq.unfold go (p1, p2)

let code = 
    let input = __SOURCE_DIRECTORY__ + ".\Day18.txt" |> System.IO.File.ReadAllLines
    input |> Array.map pline

solve code |> Seq.length