// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Lexing
open Token
open regex

let wasmtokens = "module i32 i64 f32 f64 table func local param result ( )"



[<EntryPoint>]
let main argv =
    let tokens = 
        regex.lexer "+-*'a'"
        |> Seq.map TypeOf
        |> Seq.map string
        |> Seq.map (fun (token: string) -> token.ToLower())
    
    for t in tokens do
        printfn $"{t}" 
    
    0 // return an integer exit code