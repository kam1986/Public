module regex

open Lexer
open Regex

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

let escape = !"'" => "'\\'" => char 0 .-. char 127 => !"'"
let atom = !"'" => ((char 0 .-. char 0x26 <|> char 0x28 .-. char 127) => !"'")
let plus' = !"+"
let star' = !"*"
let qm = !"?"
let or' = !"|"
let lp = !"("
let rp = !")"
let ls = !"["
let rs = !"]"
let bar = !"-"

let Atom' i (a: string) = RegexToken.Atom (byte a.[i])

type tok =
    | Atom
    | Question
    | Or
    | Star
    | Plus
    | Lpar
    | Rpar
    | Lsqrt
    | Rsqrt
    | Bar
    | EOF

let regexpattern =
    [|
        bar     != (fun _ -> Bar) --> Bar
        plus'   != (fun _ -> PLUS) --> Plus
        star'   != (fun _ -> Star) --> Star
        lp      != (fun _ -> Lpar) --> Lpar
        rp      != (fun _ -> Rpar) --> Rpar
        ls      != (fun _ -> Lsqrt) --> Lpar
        rs      != (fun _ -> Rsqrt) --> Rsqrt
        or'     != (fun _ -> Or) --> Or
        qm      != (fun _ -> Question) --> Question
        atom    != Atom' 1 --> tok.Atom
        escape  != Atom' 2 --> tok.Atom
        ""      := EOF
    |]
    |> lexer

let lexer code = LexString regexpattern code