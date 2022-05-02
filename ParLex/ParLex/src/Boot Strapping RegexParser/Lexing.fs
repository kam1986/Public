module Lexing

open Lexer
open Buffer


let escape = !"'" => !"\\" => char 0 .-. char 127 => !"'"
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
let cat = !"."

let Atom' i (a: string) = (byte a.[i])

type regtoken =
    | Atom
    | Question
    | Cat
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
        cat     := Cat
        atom    != Atom' 1 --> regtoken.Atom
        escape  != Atom' 2 --> regtoken.Atom
        eof     := EOF
    |]
    |> lexer
     
let LexString code = LexString regexpattern code

let LexFile path = 
    using // handle dispose part of the buffer
        (new LexBuffer(path: string))
        (fun buf -> LexFile regexpattern buf)