module Ref

open Wasm
open Types
open Values

let empty: int Set = set[]


let rec CollectRef instr =
    match instr with
    | RefFunc(x,_) -> set[x]
    | Loop(_, body, _)
    | Block(_, body, _) -> 
        Seq.fold 
            ( fun refs instr -> 
                CollectRef instr + refs
            ) empty body

    | If(_, fb, tb, _) -> 
        empty
        |> Seq.foldBack (fun instr refs -> CollectRef instr + refs) tb
        |> Seq.foldBack (fun instr refs -> CollectRef instr + refs) fb
        
    | Call(x, _) -> set[x]

    | _ -> empty


let CollectRefs instrs =
    Seq.fold (fun refs intr -> CollectRef intr + refs) empty instrs
    |> Set.toArray


