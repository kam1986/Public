module Generator
open Lexer
open Mapping
open DFA
open TypeAcrobatics
open Position
open Buffer
open Parser

open Token
(*

type Generator<'token when 'token : equality> = 
    val internal pattern : Position -> Map<byte seq, ('token * (string -> token) * Position * Position) * byte seq, string>
    val internal eof : 'token
    new (map, eof) = { pattern = map; eof = eof }


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
            fun (Lexer.Regex pattern) ->
                Lexing.LexRegex pattern 
                |> fun tokens ->
                    for t in tokens do
                        printfn "%A" (TypeOf t)
                    tokens
                |>Run Parsing.syntax
        ) patterns.[..patterns.Length - 2] // remove eof
        |> Array.reduce Regex.Or


    let (_, states, _) as ret = StateFinder regex
    let acceptance = getAcceptancePrState states accepts
    let table = makeTable ret acceptance

    let map = dfamap eof table : Position -> Mapping.Map<seq<byte>,(('token  * (string -> token) * Position * Position) * seq<byte>),string>
    Generator(map, eof)


let LexFile (pat: Generator<_>) (file : LexBuffer) =
    let sz = file.file.Length |> int 
    let bytes =
        seq { 0 .. sz - 1 }
        |> Seq.map (fun i -> match file.Read i with Ok b -> b | Error msg -> raise msg)
    
    let rec loop pos bytes =
        match Mapping.Run (pat.pattern pos) bytes with
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


let LexString (pat: Generator<_>) (content : string) =
    let bytes = Decoding.GetBytes content |> Array.ofList

    let rec loop pos bytes' =
        match Mapping.Run (pat.pattern pos) bytes' with
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
        | Error msg ->  seq[]

    loop (start()) bytes




let eof = Regex ""

let (!) (keyword: string) =
    Seq.map (fun c -> $"'{c}'") keyword
    |> Seq.reduce (fun word c -> $"{word}.{c}") 
    |> Regex

let (=>) (Regex reg1) (Regex reg2) = Regex $"({reg1}).({reg2})"
let (<|>) (Regex reg1) (Regex reg2) = Regex $"({reg1})|({reg2})"


let star (Regex reg) = Regex $"({reg})*"
let plus (Regex reg) = Regex $"({reg})+"
let maybe (Regex reg) = Regex $"({reg})?"



/// easy infix for ranges in regex
/// a .-. b -> ['a'-'b']
let ( .-. ) a b = Regex $"['{a}'-'{b}']" 
let ( .^. ) a b = Regex $"[^'{a}'-'{b}']"
let ( .@. ) a b = Regex $"[#'{a}'-'{b}']"



*)