module Token

open Position
open Patterns
open Wasm
open Types
open Values



    



let unsigned = Keyword "_u"
let signed  = Keyword "_s"


let i32 p = 
    Map (fun (_, op) -> I32 op) ((Keyword "i32") <&> p)

let i64 p = 
    Map (fun (_, op) -> I64 op) ((Keyword "i32") <&> p)

let f32 p = 
    Map (fun (_, op) -> F32 op) ((Keyword "i32") <&> p)

let f64 p = 
    Map (fun (_, op) -> F64 op) ((Keyword "f64") <&> p)
