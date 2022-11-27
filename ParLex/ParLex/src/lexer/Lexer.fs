module Lexer

#nowarn "25" "64"

open Mapping
open DFA
open TypeAcrobatics
open Position
open Buffer
open Regex
open Token

exception TokenError of string

// make sure counting atoms an terminals in all regex
let mutable private count = 0
let mutable private term = 0

// used to encounter possible user concurrentcy
let c() = System.Threading.Interlocked.Increment(&count)
let t() = System.Threading.Interlocked.Decrement(&term)
    

(*
    This operators makes constructing regex to make DFA's from
    easy, effecient.

    They makes it easy to format them correctly and reuse regexs as subregex in different places.

    even for quit large regexs and many patterns the compiletime of the table are low.
*)

let (!) (keyword: string) =
    System.Text.Encoding.UTF8.GetBytes keyword
    |> Seq.map (fun b -> regex.Atom(b, c()))
    |> fun s -> seq[yield! s; Epsilon] // the epsilon here will have no effect on behavour other than it will never crash
    |> Seq.reduce (fun a1 a2 -> Cat a1 a2)
    

let (=>) reg1 reg2 = Cat reg1 reg2

let (<|>) reg1 reg2 = Or reg1 reg2


let star reg  = Star reg
let plus reg  = Plus reg
let maybe reg = Or reg Epsilon

let eof = Epsilon

/// easy infix for ranges in regex
/// a .-. b -> ['a'-'b']
let inline ( .-. ) (a :'a) (b: 'a) = 
    assert(uint a < 255u && uint b < 255u)
    if a >= b then
        Epsilon
    else
        [byte a .. byte b]
        |> List.map (fun b -> regex.Atom(b, c()))
        |> List.reduce Or
    
let inline ( .^. ) a b = 
    assert(uint a < 255u && uint b < 255u)
    if a >= b then
        Epsilon
    else
        set[byte a .. byte b]
        |> (-) All
        |> Seq.map (fun b -> regex.Atom(b, c()))
        |> Seq.reduce Or
    

let inline ( .@. ) a b =     
    assert(uint a < 255u && uint b < 255u)
    if a >= b then
        Epsilon
    else
        set[byte a .. byte b]
        |> (-) ASCII
        |> Seq.map (fun b -> regex.Atom(b, c()))
        |> Seq.reduce Or


let ( != ) regex (token : 't when 't : equality, ret) = (regex, (token, fun input -> Delay ret input))
let ( := ) regex (token : 't when 't : equality) = (regex, (token, fun _ -> Arg null)) // should not be used to anything
let ( --> ) ret (token : 't when 't : equality) = (token, ret)


[<Struct>]
type Lexer<'token when 'token : equality> =
    val internal pattern : Position -> Map<byte seq, ('token * (string -> token) * Position * Position) * byte seq, string>
    val internal eof : 'token

    new (tokens : array<regex * ('token * (string -> token))>) =
        assert(tokens.Length > 0) 
        let patterns, accepts = Array.unzip tokens
        let eof =
            Array.map fst accepts
            |> Array.filter (fun token -> (string token).ToLower() = "eof")
            |> fun arr ->
                if arr.Length = 1 then
                    arr.[0]
                else
                    raise (TokenError "Missmatch in number of eof tokens") 

        let regex = // taking each pattern and making a big regex
            patterns.[..patterns.Length-2] // removing EOF pattern
            |> Array.map (fun regex -> Cat regex (Terminal (t()))) 
            |> Array.reduce (fun reg1 reg2 -> Or reg1 reg2)
             
        let (_, states, _) as ret = StateFinder regex
        let maybeAccept = getAcceptancePrState states accepts
        let table = makeTable ret maybeAccept 

        let map = dfamap eof table 

        { pattern = map; eof = eof }   

    
    
let lexer (tokens : array<regex * ('token * (string -> token))>) = Lexer(tokens)

            
let LexFile (pat: Lexer<_>) (file : LexBuffer) =
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
                seq { Token(toktype, (func (file.GetSlice(startpos.Absolut, endpos.Absolut))), startpos) }
            else
                seq { 
                    Token(toktype, func (file.GetSlice(startpos.Absolut, endpos.Absolut)), startpos) 
                    yield! loop pos bytes
                }
        | Error msg -> raise (Failure msg)

    loop (start()) bytes


let LexString (pat: Lexer<_>) (content : string) =
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