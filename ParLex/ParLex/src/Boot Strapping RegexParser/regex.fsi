module Lexing

open Buffer
open Token

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



val LexString: code : string -> token Token seq
val LexFile: path : string -> token Token seq