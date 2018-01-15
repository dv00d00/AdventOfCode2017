open System.IO
let input = File.ReadAllLines (__SOURCE_DIRECTORY__ + ".\Day4.txt")

let isValid (phrase:string) = 
    let words = phrase.Split(' ')
    let unqieWords = words |> Array.distinct
    words.Length = unqieWords.Length

let isValid2 (phrase:string) = 
    let words = phrase.Split(' ')
    let withoutAnagrams = 
        words 
        |> Array.map (fun word -> word.ToCharArray() |> Array.sort |> System.String)            
        |> Array.distinct

    words.Length = withoutAnagrams.Length


let answer1 = input |> Array.countBy isValid
let answer2 = input |> Array.countBy isValid2

