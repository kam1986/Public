module Lexer

(*

    TODO: 
       - change the conversion of the token to value to result type where we get som error formatter for the specifec case
       - Change token type and postpond the conversion of value undtil later
       - Change the iter to Some interface type that support read + seek + next
*)

#nowarn "25" "64"

open Return
open Mapping
open Regex
open DFA
open TypeAcrobatics
open Position
open Buffer
open Token

open FSharp.Collections
open System.Threading


let LexError msg =
    sprintf "Lexing Error:\n\t%s" msg
    |> Error


let ( != ) (str : string) (token : 't when 't : equality, ret) = (str, (token, fun input -> Delay ret input))
let ( := ) (str : string) (token : 't when 't : equality) = (str, (token, fun _ -> Arg null)) // should not be used to anything
let ( --> ) ret (token : 't when 't : equality) = (token, ret)


[<Struct>]
type Lexer<'token when 'token : equality> =
    val internal pattern : Position -> Map<byte seq, ('token * (string -> token) * Position * Position) * byte seq, string>
    val internal eof : 'token

    internal new (tokens : array<string * ('token * (string -> token))>, eof) =
        assert(tokens.Length > 0) 
        let mutable count = 0
        let mutable term = 0
        let patterns, accepts = Array.unzip tokens

        let regex = // taking each pattern and making a big regex
            Array.map (
                fun pattern -> 
                    Run Regex.Tokenizer <| List.map Ok (Decoding.GetBytes pattern)
                    |> debugReturn 
                    |> fun (tokens, _) -> 
                        List.map Ok tokens
                    |> Run (Parser count)
                    |> debugReturn
                    |> fun ((regex, count'), _) ->
                            count <- count'
                            term <- term - 1
                            Cat regex (Terminal term)
            ) patterns
            |> Array.reduce Or
             
        let (_, states, _) as ret = StateFinder regex
        let maybeAccept = getAcceptancePrState states accepts
        let table = makeTable ret maybeAccept 

        let map = dfamap eof table 

        { pattern = map; eof = eof }

    
    
let internal lexer (tokens : array<string * ('token * (string -> token))>) =
    let eof : 'token =
        tokens
        |> Array.map (fst << snd)
        |> Array.filter (fun (token : 'token) -> (string token).ToLower() = "eof")
        |> fun arr -> if arr.Length = 1 then arr.[0] else printfn "missmatch in number of eof tokens"; exit -1
         
    let tokens' =
        tokens
        |> Array.filter // remove the EOF pattern
            (fun x ->
                let ty = (fst << snd) x
                (string ty).ToLower() <> "eof"
            )

    Lexer(tokens', eof)

            
let LexFile pattern (file : LexBuffer) =
    let pat = lexer pattern
    let sz = file.file.Length |> int 
    let bytes =
        seq { 0 .. sz - 1 }
        |> Seq.map (fun i -> match file.Read i with Ok b -> b | Error msg -> raise msg)
    
    let rec loop pos bytes =
        match Run (pat.pattern pos) bytes with
        | Ok((toktype, func, startpos, endpos), bytes) ->
            let pos = 
                { endpos with 
                    Offset = endpos.Offset + 1
                    Absolut = endpos.Absolut + 1
                }
            
            if toktype = pat.eof then
                seq { Token(toktype, func (file.GetSlice(startpos.Absolut, endpos.Absolut)), startpos) }
            else
                seq { 
                    Token(toktype, func (file.GetSlice(startpos.Absolut, endpos.Absolut)), startpos) 
                    yield! loop pos bytes
                }
        | Error msg -> raise (Failure msg)

    loop (start()) bytes


let LexString pattern (content : string) =
    let pat = lexer pattern
    let bytes = Decoding.GetBytes content |> Array.ofList

    let rec loop pos bytes' =
        match Run (pat.pattern pos) bytes' with
        | Ok((toktype, func, startpos, endpos), bytes') -> 
            let pos = 
                { endpos with 
                    Offset = endpos.Offset + 1
                    Absolut = endpos.Absolut + 1
                }
            
            if toktype = pat.eof then
                seq { Token(toktype, func content.[startpos.Absolut .. endpos.Absolut], startpos) }
            else
                seq { 
                    Token(toktype, func content.[startpos.Absolut .. endpos.Absolut], startpos) 
                    yield! loop pos bytes'
                }
        | Error msg -> raise (Failure msg)

    loop (start()) bytes