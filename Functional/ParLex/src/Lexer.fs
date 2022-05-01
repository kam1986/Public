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
        let patterns, accepts = Array.unzip tokens
        let mutex = new System.Threading.Mutex()
        // getting the bytes
        let patterns = 
            Array.Parallel.mapi 
                (fun i p ->
                    System.Text.Encoding.UTF8.GetBytes(p : string)
                    |> Run Tokenizer
                    |> fun tokens ->
                        match tokens with
                        | Error msg -> printfn "%s" msg; exit -2                  
                        | Ok tokens -> fst tokens
                ) patterns
        
        // getting the incremental count list
        let counts = (Array.scan (+) 0 (Array.map (List.sumBy(function RegexToken.Atom _ -> 1 | _ -> 0)) patterns)).[1..]
        // getting the terminal mark list
        let terms  = [| 0 .. -1 .. -patterns.Length+1  |]

        // zip them together
        let patterns = Array.zip3 patterns counts terms
        // since count and term both are relative to the pattern
        // and the count_j - count_i >= atoms in pattern we can safely do it in parallel 
        let regex = // taking each pattern and making a big regex
            Array.map (
                fun (pattern, count, term) ->
                    printfn "term %d" term
                    // bind current count
                    pattern
                    |> Run (Parser count)
                    |> debugReturn
                    |> fun ((regex, _), _) -> Cat regex (Terminal term)
            ) patterns
            |> Array.reduce Or
        let (_, states, _) as ret = StateFinder regex
        
        let maybeAccept = getAcceptancePrState states accepts
        let table = makeTable ret maybeAccept 

        let map = dfamap eof table 

        mutex.Close()

        { pattern = map; eof = eof }

    
    
let lexer (tokens : array<string * ('token * (string -> token))>) =
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

            
let LexFile (pat : Lexer<_>) (file : LexBuffer) =
    // get number of bytes
    let sz = file.file.Length |> int 

    // create a persistent sequence of the bytes with caching in the underlying buffer
    let bytes =
        seq { 0 .. sz - 1 }
        |> Seq.map (fun i -> match file.Read i with Ok b -> b | Error msg -> raise msg)
    
    // loop through the bytes to find all tokens given by pat
    let rec loop pos bytes =
        match Run (pat.pattern pos) bytes with
        | Ok((toktype, func, startpos, endpos), bytes) ->
            let pos = 
                { endpos with 
                    Offset = endpos.Offset + 1
                    Absolut = endpos.Absolut + 1
                }
            // check for end of file
            if toktype = pat.eof then
                // propagate end of file token to the end of the sequence
                seq { Token(toktype, func (file.GetSlice(startpos.Absolut, endpos.Absolut)), startpos) }
            else
                // add the token to the relativ front of the sequence and loop the rest of the bytes
                seq { 
                    Token(toktype, func (file.GetSlice(startpos.Absolut, endpos.Absolut)), startpos) 
                    yield! loop pos bytes
                }
        // some lexing error happened
        | Error msg -> raise (Failure msg)
    
    // return the sequence of tokens
    loop (start()) bytes


let LexString (pat : Lexer<_>) (content : string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(content)
    let str bytes = System.Text.Encoding.UTF8.GetString(bytes: byte[]) 
    
    printfn "bytes %d" bytes.Length

    let rec loop pos bytes' =
        match Run (pat.pattern pos) bytes' with
        | Ok((toktype, func, startpos, endpos), bytes') -> 
            let pos = 
                { endpos with 
                    Offset = endpos.Offset + 1
                    Absolut = endpos.Absolut + 1
                }
            
            if toktype = pat.eof then
                seq { Token(toktype, func (str bytes.[startpos.Absolut .. endpos.Absolut]), startpos) }
            else
                seq { 
                    Token(toktype, func (str bytes.[startpos.Absolut .. endpos.Absolut]), startpos) 
                    yield! loop pos bytes'
                }
        | Error msg -> raise (Failure msg)

    loop (start()) bytes