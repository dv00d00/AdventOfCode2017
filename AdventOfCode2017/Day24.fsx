open System.Threading.Tasks
open System
open System.Threading

type Part = int * int
type Chain = Part list
type Pool = Set<Part>

let actualInput = [ 
    (48,5); (25,10); (35,49); (34,41); (35,35); (47,35); (34,46); (47,23); (28,8); (27,21); (40,11); (22,50); (48,42); (38,17);
    (50,33); (13,13); (22,33); (17,29); (50,0); (20,47); (28,0); (42,4); (46,22); (19,35); (17,22); (33,37); (47,7); (35,20);
    (8,36); (24,34); (6,7); (7,43); (45,37); (21,31); (37,26); (16,5); (11,14); (7,23); (2,23); (3,25); (20,20); (18,20); (19,34);
    (25,46); (41,24); (0,33); (3,7); (49,38); (47,22); (44,15); (24,21); (10,35); (6,21); (14,50);
]

let test = [(0,2); (2,2); (2,3); (3,4); (3,5); (0,1); (10,1); (9,10);]

let sum (parts:Chain) = 
    if parts = [] then 0 else
    parts |> List.sumBy (fun (a,b) -> a + b)

let matches head (sideA, sideB) = 
    match head with
    | Some (start,``end``) -> ``end`` = sideA
    | _ -> true

let result set = seq {
    
    let rec go (chain:Chain) (pool:Pool) = seq {

        let head = chain |> List.tryHead      

        if pool <> Set.empty then
            for part in pool do

                if matches head part then
                    let pool = pool |> Set.remove part
                    yield! go (part::chain) pool

                let flipped = let (a,b) = part in (b,a)
                if matches head flipped then 
                    let pool = pool |> Set.remove part
                    yield! go (flipped::chain) pool

        yield chain
    }

    let starts = set |> List.filter (fun (a,b) -> a = 0 || b = 0)
    let pool = set |> Set.ofList

    for start in starts do
        let head = 
            let a,b = start
            in if a = 0 then start else (b,a)

        let pool = pool |> Set.remove start

        yield! (go [head] pool) |> Seq.map (List.rev)
    }
    

result test 
|> Seq.maxBy sum

let results = result actualInput 

let answer = results |> Seq.maxBy sum 
sum answer

let answer2 = results |> Seq.maxBy (fun it -> (it |> List.length, sum it))
sum answer2


answer.Length
answer2.Length









open System.Threading.Tasks
open System
open System.Threading

type Worker = Uri*CancellationToken -> Task<string>

let worker: Worker = 
    fun (uri,ct) ->
        Task.FromResult("Hey")
type Decorator = Worker -> Worker
let decorate:Decorator = fun fn -> fn

type Logged<'x> = Logged of 'x
type Monitored<'x> = Monitored of 'x
type RetrySettings = Settings of int
type Retried<'x> = Retried of 'x * RetrySettings
type PicLogged<'x> = PicLogged of 'x

let withLogging worker = 
    fun arg ->
        try
            printfn "hello"
            let result = worker arg
            printfn "world"
            Logged result
        with e -> 
            printfn "error: %A" e
            reraise ()

let withMonitoring worker = 
    fun arg ->
        try
            printfn "hello"
            let result = worker arg
            printfn "world"
            Monitored result
        with e -> 
            printfn "error: %A" e
            reraise ()

let withRetry retrySettings worker  = 
    fun arg  ->
        try
            printfn "hello"
            let result = worker arg
            printfn "world"
            Retried (result, retrySettings)
        with e -> 
            printfn "error: %A" e
            reraise () 

worker(Uri("test"), CancellationToken.None)

let worker2 = decorate worker

worker2(Uri("test"), CancellationToken.None)

let logged = withLogging worker2

logged(Uri("test"), CancellationToken.None)


let defaultDecorator settings worker = 
    worker
    |> withRetry settings 
    |> withMonitoring
    |> withLogging
    

let stack = defaultDecorator (Settings 4) worker
stack (Uri("test"), CancellationToken.None)


let test22 x = x + 1
let test33 = defaultDecorator (Settings 4) test22
test33 10