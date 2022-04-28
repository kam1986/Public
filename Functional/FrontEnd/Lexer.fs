module Lexer

open System.IO
open Lexer
open Buffer
open Token
open Tokens

let (<|>) reg1 reg2 = $"({reg1})|({reg2})"
let (<&>) reg1 reg2 = $"{reg1}{reg2}"

let star reg  = $"({reg})*"
let plus reg  = $"({reg})+"
let maybe reg = $"({reg})?"


let digit    = "[0-9]"
let hexdigit = "[a-f]" <|> "[A-F]" <|> digit

let sign = maybe ("\+" <|> "\-")
let ubar = maybe "_"
let num = sign <&> (plus digit <&> ubar |> star) <&> plus digit

let hexnum = ((plus hexdigit) <&> ubar |> star) <&> plus hexdigit

let frac = star (digit <|> (digit <&> ubar))
let hexfrac = star (hexdigit <|> (hexdigit <&> ubar))


let noise = plus (" " <|> "\n" <|> "\t" <|> "\r")

// removed character " and \
let byte  = "[\x00-\xFF]"
let one   = $"[{char 0b0000_0000}-{char 0b0111_1111}]" // escape char and such are check later
let two   = $"[{char 0b1000_0000}-{char 0b1011_1111}]{byte}" 
let three = $"[{char 0b1100_0000}-{char 0b1101_1111}]{byte}{byte}"
let four  = $"[{char 0b1110_0000}-{char 0b1110_1111}]{byte}{byte}{byte}"

// unicode
let utf8 = star (one <|> two <|> three <|> four)

let fnum = (num <&> maybe ("." <&> maybe frac) <&> ("E"<|> "e") <&> sign <&> num)

let fhex = "0x" <&> ((hexnum <&> maybe ("." <&> maybe hexfrac) <|> maybe (("P"<|> "p") <&> sign <&> num)))

let float str =
    if str = "nan" then
        nan
    elif str = "infinity" || str = "inf" || str = "+infinity" || str = "+inf" then
        infinity
    elif str = "-infinity" || str = "-inf" then
        -infinity
    else
        float str


// lexical patterns with transformation functions and token identifier
let pattern = 
    [|
        "let"                       := LET
        "true"                      := TRUE
        "false"                     := FALSE
        "i8"                        := I8
        "i16"                       := I16
        "i32"                       := I32
        "i64"                       := I64
        "u8"                        := U8
        "u16"                       := U16
        "u32"                       := U32
        "u64"                       := U64
        "f32"                       := F32
        "f64"                       := F64
        "while"                     := WHILE
        "return"                    := RETURN
        "break"                     := BREAK
        "continue"                  := CONTINUE
        "if"                        := IF
        "else"                      := ELSE
        "elif"                      := ELIF
        "then"                      := THEN
        "&"                         := AND
        "\|"                        := OR
        "\^"                        := XOR
        ":"                         := COLON
        ";"                         := SEMICOLON
        ","                         := COMMA
        ">>"                        := SHIFTLEFT
        "<<"                        := SHIFTRIGHT
        "<o"                        := ROTATERIGHT
        "o>"                        := ROTATELEFT
        "%"                         := REMINDER
        "\+"                        := PLUS
        "\-"                        := MINUS
        "\*"                        := STAR
        "\/"                        := SLASH
        "="                         := EQUAL
        "<>"                        := NOT_EQUAL
        "<"                         := LESS 
        ">"                         := GREATER
        "<="                        := LEQ
        ">="                        := GEQ
        "<\-"                       := ASSIGN
        "\->"                       := IMPLY
        "\("                        := LPAR
        "\)"                        := RPAR
        "{"                         := LBRA
        "}"                         := RBRA
        "\["                        := LSQRT
        "\]"                        := RSQRT
        "max"                       := MAX
        "min"                       := MIN
        "copysign"                  := COPYSIGN
        num                         != int64 --> INTEGER
        fnum                        != float --> FLOAT
        noise                       := NOISE
        $"\"{utf8}\""               != (fun (str: string) -> str.[1 .. str.Length - 2]) --> STRING
        "[a-zA-Z][a-zA-Z0-9_]*"     != id --> ID
        ""                          := EOF
    |]
    


let LexFile (path : string) =
    let buf = new LexBuffer(path)
    LexFile pattern buf
    |> Seq.filter (fun token -> TypeOf token <> NOISE)

let LexString str = 
    LexString pattern str 
    |> Seq.filter (fun token -> TypeOf token <> NOISE)

