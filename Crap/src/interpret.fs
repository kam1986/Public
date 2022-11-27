module interpret
(*
open System.Numerics

open Syntax
open Table

exception IntrepetationError of Message: string

let IError msg info =
    IntrepetationError $"Runtime Error: {msg} at {info.pos}"
    |> raise



let rec InterpretExpr vtab e =
    match e with
    | Value _ -> e
    | Variable(name, info) ->
        match Lookup name vtab with
        | None -> IError $"the value {name} are not defined" info
        | Some v -> v

    | Unary(op, e, info) ->
        let (Value(v, _)) = InterpretExpr vtab e
        match op with
        | Neg -> 
            match v with
            | I32 i -> Value(I32 -i, info)
            | F64 f -> Value(F64 -f, info)

        | Not ->
            match v with
            | I32 i -> Value(I32 (i ^^^ -1), info)
            | F64 f -> IError "not not defined for f64" info

        | Pc ->
           match v with
           | I32 i -> Value(I32 (BitOperations.PopCount(uint i)), info)
           | F64 f -> IError "popcnt not defined for f64" info 
            
        | Clz ->
            match v with
            | I32 i -> Value(I32 (BitOperations.LeadingZeroCount(uint i)), info)
            | F64 f -> IError "popcnt not defined for f64" info 

        | Ctz ->
            match v with
            | I32 i -> Value(I32 (BitOperations.TrailingZeroCount(uint i)), info)
            | F64 f -> IError "popcnt not defined for f64" info 

        | Sqrt -> 
            match v with
            | I32 i -> Value(I32 ((int << sqrt << float) i), info)
            | F64 f -> Value(F64 (sqrt f), info)

        | Floor ->
            match v with
            | I32 i -> Value(I32 i, info)
            | F64 f -> Value(F64 (floor f), info)

        | Ceil  ->
            match v with
            | I32 i -> Value(I32 i, info)
            | F64 f -> Value(F64 (ceil f), info)

        | Abs ->
            match v with
            | I32 i -> Value(I32 (abs i), info)
            | F64 f -> Value(F64 (abs f), info)

    | Binary(op', left, right, info) ->
        let left = InterpretExpr vtab left
        let right = InterpretExpr vtab right
        match left, right with
        | Value(v1, _), Value(v2, _) -> 
            let inline f a b = 
                match op' with 
                | Add -> (+) a b | Sub -> (-) a b 
                | Mul -> (*) a b | Div -> (/) a b

            match v1, v2 with
            | I32 v1, I32 v2 -> Value(I32 (f v1 v2), info)
            | F64 v1, F64 v2 -> Value(F64 (f v1 v2), info)
            | _ -> failwith $"{pptype v1} is not the same type as {pptype v2}"

        | _ -> failwith "the expression does not minimize"

    | Convert(e, info) ->
        match e with
        | I32 e -> 
            match InterpretExpr vtab e with
            | Value(v,_)  -> 
                match v with
                | I32 _ -> Value(v, info)
                | F64 f -> Value(I32(int f), info)

            | _ -> failwith "at convert should not happen"

        | F64 e ->
            match InterpretExpr vtab e with
            | Value(v, _) -> 
                match v with
                | I32 i -> Value(F64(float i), info)
                | F64 _ -> Value(v, info)
            | _ -> failwith "at convert should not happen"


let rec InterpretStmt vtab stmt =
    match stmt with
    | Dec(name, body, info) ->
        let v = 
            if info.mut then
                SetMut <| InterpretExpr vtab body
            else
                InterpretExpr vtab body

        Bind name v vtab, []

    | Assign(name, body, info) ->
        match Lookup name vtab with
        | None -> IError $"the value {name} are not defined" info
        | Some v ->
            if (GetInfoExpr v).mut then
                let v = SetMut (InterpretExpr vtab body)
                Bind name v vtab, []
            
            else
                IError $"the value {name} are immutable" info
     
    | Ret(e, info) ->
        vtab, [InterpretExpr vtab e]


*)