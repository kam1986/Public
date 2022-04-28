module ParserGenerator
(*
open System
(*

Implementation Notes

    - A state in a PDFA is a function with some carry function ad accumulator and implicit stack 
    - as with the LexerGenerator we first find all state and transitions of the underlying DFA
    - we then compute the needed carry functions
    - then combine those
    - format it in F# code and
    - write it to output file (.fs file)

*)

// the carry function is normally a function that takes some array of arguments 'a -> .. -> b' and passe it into a function f : ('a * .. * 'b) 
// here we expand the definition a bit by saying that any carrying of some function 'func' take at least some argument 'arg' and return the value of that 


//        1       2      3     4
// E -> E + E | E * E | (E) | Val

let carry arg f = f arg
let carry2 arg1 arg2 f

type token = Id of int | PLUS | TIMES | LPAR | RPAR 

type exp =
    | Val of int
    | Add of exp * exp
    | Mul of exp * exp

let Val (Id arg) = Val arg
let Add arg1 arg2 = Add(arg1,arg2)
let Mul arg1 arg2 = Mul(arg1,arg2)

let rec state0 input =
    match input with
    | (Id _ as id) :: input -> state3 (carry2 (Val id)) input
    | LPAR :: input -> state2 input
    | _ -> failwith "parser error state0"

and state1 acc input =
    match input with
    | [] -> acc
    | PLUS  :: input -> state4 (Add acc) input
    | TIMES :: input -> state5 (Mul acc) input
    | _ -> failwith "parser error state1"
    

and state2 input =
    match input with
    | (Id _ as id) :: input -> state3 (carry2 (Val id)) input
    | LPAR :: input -> state2 input
    | _ -> failwith "parser error state2"


and state3 acc input =
    match input with
    | [] 
    | PLUS :: input 
    | TIMES :: input 
    | RPAR :: input -> 
    | _ -> failwith "Parser error state3"

and state4 input =
    match input with
    | (Id _ as id) :: input -> state3 (carry2 (Val id)) input
    | LPAR :: input -> state2 input
    | _ -> failwith "parser error state0"


and state5 input =
    match input with
    | (Id _ as id) :: input -> state3 (carry2 (Val id)) input
    | LPAR :: input -> state2 input
    | _ -> failwith "parser error state0"


and state6 input

and state7 input

and state8 input

and state9 input
*)