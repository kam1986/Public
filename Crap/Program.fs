// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

open lexer
open parser
open interpret
open Syntax
open Token
open Table



[<EntryPoint>]
let main _ =
    "fun test arg1 arg2 = arg1 + arg2\n    (1 + 2)"
    |> LexCrap
    |> ParseCrap
    |> printfn "%A"
    0 // return an integer exit code