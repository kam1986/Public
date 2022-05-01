// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open regex
open Token
open Regex
open Parsing
open Parser

let reg = "['0'-'9']-['2'-'4']"



[<EntryPoint>]
let main argv =
    let tokens = (lexer reg)
    for token in Seq.map TypeOf tokens do
        printfn "%A" token

    let regex = 
        Parser.Run syntax tokens
        |> fun ret ->
            match ret with
            | Error msg -> 
                printfn $"{msg}"
                
            | Ok b -> 
                printfn "%A" (b :> regex)
    


    0 // return an integer exit code