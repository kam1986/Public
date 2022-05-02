// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Lexing
open Token
open Regex
open Parsing
open Parser

let reg = "['a'-'z']"



[<EntryPoint>]
let main argv =
    let tokens = (LexString reg)
    for token in Seq.map TypeOf tokens do
        printfn "%A" token

    //let regex = 
    //    Parser.Run syntax tokens
        
    //printfn "%A" (regex :> regex)
    


    0 // return an integer exit code