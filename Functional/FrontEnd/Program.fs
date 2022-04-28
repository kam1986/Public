// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Lexer
open Token
open Regex
open Mapping


[<EntryPoint>]
let main _ =
    let test = "123"
    let result = LexString test
    printfn $"Tokens {ValueOf <| Seq.head  result}" 
    
   
    0 // return an integer exit code