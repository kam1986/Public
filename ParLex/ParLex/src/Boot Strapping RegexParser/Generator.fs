module Generator

open Return
open Mapping
open Regex
open DFA
open TypeAcrobatics
open Position
open Buffer
open Lexer
open Lexing
open Parsing
open Parser
open Token

exception TokenError of string


// bootstrapping lexer of regular expressions for better speed
let GenLexer patterns =
    let patterns, accepts = Array.unzip patterns
    let eof =
        Array.map fst accepts
        |> Array.filter (fun token -> (string token).ToLower() = "eof")
        |> fun arr ->
            if arr.Length = 1 then
                arr.[0]
            else
                raise (TokenError "Missmatch in number of eof tokens")

    let regex =
        Array.map(
            fun (Regex pattern) ->
                LexString pattern
                |> Parser.Run Parsing.syntax
        ) patterns
        |> Array.reduce Regex.Or

    let (_, states, _) as ret = StateFinder regex
    let acceptance = getAcceptancePrState states accepts
    let table = makeTable ret acceptance

    let map = dfamap eof table
    Lexer(map, eof)



