module Lexing

open Lexer
open Regex
open Buffer

type regex = string

// make atoms of each element of a string
let (!) (reg: string) : regex = Seq.fold (fun reg r -> $"{reg}\\{r}") "" reg

let (<|>) (reg1: regex) (reg2: regex): regex = 
    if reg1.Length = 0 then
        reg2
    elif reg2.Length = 0 then
        reg1
    elif reg1.Length = 1 then
        if reg2.Length = 1 then
            $"\\{reg1}|\\{reg2}"
        else
            $"\\{reg1}|({reg2})"
    elif reg2.Length = 1 then 
        $"({reg1})|\\{reg2}"
    else
        $"({reg1})|({reg2})"

let (=>) (reg1: regex) (reg2: regex): regex = 
    if reg1.Length = 0 then
        reg2
    elif reg2.Length = 0 then
        reg1
    elif reg1.Length = 1 then
        if reg2.Length = 1 then
            $"\\{reg1}|\\{reg2}"
        else
            $"\\{reg1}|({reg2})"
    elif reg2.Length = 1 then 
        $"({reg1})|\\{reg2}"
    else
        $"({reg1})({reg2})"

    
let maybe (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"\\{reg}?"
    else
        $"({reg})?"

let star (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"\\{reg}*"
    else
        $"({reg})*"


let plus (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"\\{reg}+"
    else
        $"({reg})+"

/// easy infix for ranges in regex
/// a .-. b -> ['a'-'b']
let ( .-. ) a b : regex = $"[\\{a}-\\{b}]" 
let ( .^. ) a b : regex = $"[^\\{a}-\\{b}]"
let ( .@. ) a b : regex = $"[#\\{a}-\\{b}]"

let escape = !"'" => "'\\'" => char 0 .-. char 127 => !"'"
let atom = !"'" => ((char 0 .-. char 0x26 <|> char 0x28 .-. char 255) => !"'")
let plus' = !"+"
let star' = !"*"
let qm = !"?"
let or' = !"|"
let lp = !"("
let rp = !")"
let ls = !"["
let rs = !"]"
let bar = !"-"
let hat = !"^"
let numb = !"#"

let Atom' i (a: string) = (byte a.[i])

type token =
    | Atom
    | Question
    | Or
    | Star
    | Plus
    | Hat
    | Lpar
    | Rpar
    | Lsqrt
    | Rsqrt
    | Bar
    | Numb
    | EOF

let regexpattern =
    [|
        bar     := Bar
        plus'   := Plus
        star'   := Star
        lp      := Lpar
        rp      := Rpar
        ls      := Lsqrt
        rs      := Rsqrt
        or'     := Or
        hat     := Hat
        numb    := Numb
        qm      := Question
        atom    != Atom' 1 --> token.Atom
        escape  != Atom' 2 --> token.Atom
        ""      := EOF
    |]
    |> lexer
     
let LexString code = LexString regexpattern code

let LexFile path = 
    using // handle dispose part of the buffer
        (new LexBuffer(path: string))
        (fun buf -> LexFile regexpattern buf)