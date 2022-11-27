module TypeChecking

open Ast
open Typer
open Miljø
open Result


let TypeAf værdi =
    match værdi with
    | I8  _ -> I8 
    | I16 _ -> I16
    | I32 _ -> I32
    | I64 _ -> I64
    | U8  _ -> U8 
    | U16 _ -> U16
    | U32 _ -> U32
    | U64 _ -> U64
    | F32 _ -> F32
    | F64 _ -> F64 
    |> fun op -> op()
    |> fun t -> App(Type t, [])


let TypeAfUdtryk (udtryk) =
    match udtryk with
    | Værdi(_, info: {| ty : _ Type; Pos: Position|})
    | Variable(_, info)
    | Unær(_,_, info)
    | Binær(_,_,_,info)
    | udtryk.Kald(_,_,info)
    | Læs(_,_,info) -> info.ty

let TypeAfUdSagn udsagn =
    match udsagn with
    | Tildel(_,_,info: {| ty : _ Type; Pos: Position|})
    | Ændre(_,_, info)
    | Hvis(_,_,_,info)
    | Kald(_,_, info) 
    | Mens(_,_, info)
    | Skriv(_,_,info)
    | Resultat(_,info) -> info.ty



let rec TjekUdtryk miljø udtryk' =
    match udtryk' with
    | Værdi (v, info: {| Pos: Position |}) -> Ok (Værdi(v, {| info with ty = TypeAf v |}))
    | Variable(navn, info) ->
        FindVærdi navn miljø
        |> Result.map (fun ty -> Variable(navn, {| info with ty = ty |}))

    | Unær(op, udtryk', info) ->
        TjekUdtryk miljø udtryk'
        |> Result.mapWithError (fun udtryk ->
            let ty = TypeAfUdtryk udtryk
            let opt = TypeAf op
            if ty = opt then
                Ok(Unær(op, udtryk, {| info with ty = ty |}))
            else
                Error "invalid binary opeation"
            )

    | Binær(op, venstre, højre, info) ->
        TjekUdtryk miljø venstre
        |> Result.mapWithError (fun venstre' ->
            TjekUdtryk miljø højre
            |> Result.map (fun højre' -> venstre', højre' ) 
        )
        |> Result.mapWithError (fun (venstre, højre) ->
            let vty = TypeAfUdtryk venstre
            let hty = TypeAfUdtryk højre
            let oty = TypeAf op
            if vty <> hty then
                Error $"{vty} er ikke forenelig med {hty}"
            elif oty <> vty then
                Error $"{oty} er ikke forenelig med {vty}"
            else
                Ok(Binær(op, venstre, højre, {| info with ty = vty |}))
        )

    | udtryk.Kald(navn, argumenter, info) ->
       List.map (TjekUdtryk miljø) argumenter
       |> List.fold (Result.map2 (fun args arg -> arg :: args)) (Ok []) 
       |> Result.map List.rev
       |> Result.map (fun args ->
            FindFunktion navn miljø
            |> Result.mapWithError (fun (args', ret) -> 
                if args' = List.map TypeAfUdtryk args then 
                    Ok(args, ret) 
                else 
                    Error "Argument type passer ikke"
               )
            |> Result.map (fun (args, ret) -> udtryk.Kald(navn, args, {| info with ty = ret |}))
            )
       |> Result.mapWithError id
