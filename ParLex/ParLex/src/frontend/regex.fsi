module Lexing

open Buffer
open Token

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


val LexString: code : string -> regtoken Token seq
val LexFile: path : string -> regtoken Token seq