#r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"

open FParsec

type Data = Group of Data list | Garbage

let pbang: Parser<_,unit> = 
    pchar '!' >>. anyChar

let pgarbage: Parser<_,unit> = 
    pchar '<' .>> many (pbang <|> satisfy (fun c -> c <> '>')) .>> pchar '>' 
    |>> fun _ -> Garbage

let pgroup, pgroupref = createParserForwardedToRef<Data, unit>()

do pgroupref := 
    let start = pchar '{'
    let end' = pchar '}'
    let comma = pchar ','
    let inner = pgroup <|> pgarbage
    start >>. ((sepBy inner comma) |>> Group) .>> end'

let input = __SOURCE_DIRECTORY__ + ".\Day9.txt" |> System.IO.File.ReadAllText



let rec score (acc: int) (data:Data) = 
    match data with
    | Garbage -> 0
    | Group(inner) -> 
        let children = inner |> List.map (score (acc + 1)) |> List.sum
        children + acc

module Counter = 
    
    open FParsec

    let const' x = fun _ -> x

    type Data = Group of Data list | Garbage

    let pbang: Parser<_,unit> = 
        pchar '!' >>. anyChar |>> const' 0

    let pgarbage: Parser<_,unit> = 
        let one = satisfy (fun c -> c <> '>') |>> const' 1
        let hz = (many (pbang <|> one)) |>> fun xs -> xs |> List.sum
        pchar '<' >>.  hz  .>> pchar '>'         

    let pgroup, pgroupref = createParserForwardedToRef<int, unit>()

    do pgroupref := 
        let start = pchar '{'
        let end' = pchar '}'
        let comma = pchar ','
        let inner = pgroup <|> pgarbage
        start >>. ((sepBy inner comma) |>> fun xs -> xs |> List.sum) .>> end'

    let (Success(res, _, _)) = run pgroup "{}"

    let (xs:int list) = []
    List.sum xs

    input.Length