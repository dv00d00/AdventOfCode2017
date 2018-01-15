open System.Collections.Generic

(*
Begin in state A.
Perform a diagnostic checksum after 12523873 steps.
*)

type Value = One | Zero
type StateId = char
type State = {
    ID: StateId

    IfZero: Transition
    IfOne: Transition
}
and Transition = { Write: Value; MoveTo: Direction; NextState: StateId }
and Direction = Left | Right

let A = { ID = 'A'; IfZero = { Write = One; MoveTo = Right; NextState = 'B' }; IfOne = { Write = One; MoveTo = Left; NextState = 'E' } }
let B = { ID = 'B'; IfZero = { Write = One; MoveTo = Right; NextState = 'C' }; IfOne = { Write = One; MoveTo = Right; NextState = 'F' } }
let C = { ID = 'C'; IfZero = { Write = One; MoveTo = Left;  NextState = 'D' }; IfOne = { Write = Zero; MoveTo = Right; NextState = 'B' } }
let D = { ID = 'D'; IfZero = { Write = One; MoveTo = Right; NextState = 'E' }; IfOne = { Write = Zero; MoveTo = Left; NextState = 'C' } }
let E = { ID = 'E'; IfZero = { Write = One; MoveTo = Left;  NextState = 'A' }; IfOne = { Write = Zero; MoveTo = Right; NextState = 'D' } }
let F = { ID = 'F'; IfZero = { Write = One; MoveTo = Right; NextState = 'A' }; IfOne = { Write = One; MoveTo = Right; NextState = 'C' } }

let code = [ A;B;C;D;E;F ] |> List.map (fun x -> x.ID, x) |> Map.ofList

let inline tryGet key deflt (tape:Dictionary<_, _>) = 
    if tape.ContainsKey key then tape.[key]
    else deflt

let run () = 
    let tape = new Dictionary<int, Value>()
    let mutable curPos = 0
    let mutable curState = 'A'    

    for i = 1 to 12523873 do
        let state = code.Item curState
        let value = tape |> tryGet curPos Zero
        
        let transition = 
            if value = Zero 
                then state.IfZero 
                else state.IfOne

        tape.[curPos] <- transition.Write

        if transition.MoveTo = Left then
            curPos <- curPos - 1
        else
            curPos <- curPos + 1

        curState <- transition.NextState

    tape |> Seq.sumBy (fun x -> if x.Value = One then 1 else 0)

run ()
    