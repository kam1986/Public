module public Lexing

open Position
open Lexer
open Tokens
open Wasm
open Types

type regex = string


// make atoms of each element of a string
let (!) (reg: string) : regex = Seq.fold (fun reg r -> $"{reg}'{r}'") "" reg

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
let ( .-. ) a b : regex = $"['{a}'-'{b}']" 

let digit = 0 .-. 9 
let sign = maybe ("+" <|> "-") 

let num = plus (digit => maybe ("_" => digit))

let hexdigit = digit <|> 'a' .-. 'f' <|> 'A' .-. 'F'

let hexnum = plus (hexdigit => maybe (!"_" => hexdigit))

let un = num <|> (!"0x" => hexnum)
let sn = sign => un

let flt = 
    num => (maybe ("." => maybe num) => maybe (("E" <|> "e") => sign => num))

let hflt =
    !"0x" => 
    hexnum => 
    (maybe ("." => maybe hexnum) => maybe (("P" <|> "p") => sign => hexnum))
    
let fn = sign => (hflt <|> flt <|> !"inf" <|> !"nan" <|> (!"nan:0x" => hexnum))


let escape =
    "\\" => ("t" <|> "t" <|> "\"" <|> "'" <|> "\\" <|> (!"u{" => hexnum => "}")) 


let chr =
    // the interval of bytes a trailing byte can have
    let trailing = char 0b1000000 .-. char 0b10111111
    
    // pattern for 1 byte long utf8 char, with " and other characters removed 
    let one  = (string <| char 0x20) <|> (string <| char 0x21) <|> char 0x23 .-. char 0x5B <|> char 0x5D .-. char 0x7E  
    
    // two bytes utf8 char
    let two  = char 0b11000000 .-. char 0b11011111 => trailing
    
    // tree bytes utf8 char
    let tree = char 0b11100000 .-. char 0b11101111 => trailing => trailing
    
    // four bytes utf8 char
    let four = char 0b11110000 .-. char 0b11110111 => trailing => trailing => trailing

    // the combined possible pattern
    one <|> two <|> tree <|> four <|> escape



let str = "\"" => star chr => "\""

let id = 
    let spc = 
        Seq.fold (fun regex c -> string c <|> regex) "" "!#$%&'*+-./:<=>?@\^_`|~"
    "$" => plus (digit <|> 'a' .-. 'z' <|> 'A' .-. 'Z' <|> spc) 

let whitespace = plus (" "<|> "\n" <|> "\r" <|> "\t")


let pat =
    [|
        !"i32"          != I32 --> WT.I32
        !"i64"          != I64 --> WT.I64
        !"f32"          != I32 --> WT.F32
        !"f64"          != I64 --> WT.F64
        !"funcref"      := FUNCREF
        !"externref"    := EXTERNREF
        !"func"         := FUNC
        !"extern"       := EXTERN
        !"param"        := PARAM
        !"result"       := RESULT
        !"local"        := LOCAL
        !"start"        := START
        !"import"       := IMPORT
        !"export"       := EXPORT
        !"table"        := TABLE
        !"global"       := GLOBAL
        !"memory"       := MEMORY
        !"mut"          := MUT
        !"loop"         := LOOP
        !"block"        := BLOCK
        !"if"           := IF
        !"else"         := ELSE
        !"end"          := END
        !"const"        := CONST
        !"is_null"      := ISNULL
        !"null"         := NULL
        !"drop"         := DROP
        !"elem"         := ELEM
        !"select"       := SELECT
        !"tee"          := TEE
        !"set"          := SET
        !"get"          := GET
        !"copy"         := COPY
        !"fill"         := FILL
        !"size"         := SIZE
        !"init"         := INIT
        !"grow"         := GROW
        !"data"         := DATA
        !"load"         := LOAD
        !"store"        := STORE
        !"load8"        := LOAD8
        !"store8"       := STORE8
        !"load16"       := LOAD16
        !"store16"      := STORE16   
        !"load32"       := LOAD32
        !"store32"      := STORE32   
        !"offset"       := OFFSET
        !"_"            := UNDERSCORE
        !"_s"           := SIGNED
        !"_u"           := UNSIGNED
        !"clz"          := CLZ
        !"ctz"          := CTZ
        !"popcnt"       := POPCNT
        !"add"          := ADD
        !"sub"          := SUB
        !"mul"          := MUL
        !"div"          := DIV
        !"rem"          := REM
        !"and"          := AND
        !"or"           := OR
        !"xor"          := XOR
        !"shl"          := SHL
        !"shr"          := SHR
        !"rotl"         := ROTL
        !"rotr"         := ROTR
        !"abs"          := ABS
        !"neg"          := NEG
        !"ceil"         := CEIL
        !"floor"        := FLOOR
        !"trunc"        := TRUNC
        !"nearest"      := NEAREST
        !"sqrt"         := SQRT
        !"min"          := MIN
        !"max"          := MAX
        !"copysign"     := COPYSIGN
        !"sat"          := SAT
        !"eqz"          := EQZ
        !"eq"           := EQ
        !"ne"           := NE
        !"ge"           := GE
        !"gt"           := GT
        !"le"           := LE
        !"lt"           := LT
        !"wrap"         := WRAP
        !"then"         := THEN
        !"convert"      := CONVERT
        !"demote"       := DEMOTE
        !"promote"      := PROMOTE
        !"reintepret"   := REINTERPRET
        !"type"         := TYPE
        // we convert a unsigned at parse type
        // all occurencess of a number are translated into 64bits
        // and will be transformed to proper size later
        !"."            := DOT
        !"("            := LPAR
        !")"            := RPAR
        whitespace      := WHITESPACE 
        fn              != float --> FLOAT
        id              != (fun a -> a) --> ID
        str             != (fun (str: string) -> str.[1..str.Length-2]) --> STR
        un              != uint --> UINT
        sn              != int  --> INT 
        ""              := EOF
    |]
    |> lexer


let lexer code = LexString pat code
               


