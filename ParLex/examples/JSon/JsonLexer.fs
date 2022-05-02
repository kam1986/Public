module JsonLexer


open Parsing
open Lexer


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
            $"'{reg1}'|'{reg2}'"
        else
            $"'{reg1}'|({reg2})"
    elif reg2.Length = 1 then 
        $"({reg1})|'{reg2}'"
    else
        $"({reg1})|({reg2})"

let (=>) (reg1: regex) (reg2: regex): regex = 
    if reg1.Length = 0 then
        reg2
    elif reg2.Length = 0 then
        reg1
    elif reg1.Length = 1 then
        if reg2.Length = 1 then
            $"'{reg1}''{reg2}'"
        else
            $"'{reg1}'({reg2})"
    elif reg2.Length = 1 then 
        $"({reg1})'{reg2}'"
    else
        $"({reg1})({reg2})"

    
let maybe (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"'{reg}'?"
    else
        $"({reg})?"

let star (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"'{reg}'*"
    else
        $"({reg})*"


let plus (reg: regex) : regex = 
    if reg.Length = 0 then
        ""
    elif reg.Length = 1 then
        $"'{reg}'+"
    else
        $"({reg})+"

/// easy infix for ranges in regex
/// a .-. b -> ['a'-'b']
let ( .-. ) a b : regex = $"[\\{a}-\\{b}]" 
let ( .^. ) a b : regex = $"[^\\{a}-\\{b}]"
let ( .@. ) a b : regex = $"[#\\{a}-\\{b}]"


