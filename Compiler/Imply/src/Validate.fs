module Validate

open Abs
open Type

type 'o Option with
    static member toResult err ret =
        match ret with
        | None -> Error err
        | Some ret -> Ok ret

let TypeOfOp v =
    match v with
    | I8  _ -> I8() 
    | I16 _ -> I16() 
    | I32 _ -> I32() 
    | I64 _ -> I64() 
    | U8  _ -> U8() 
    | U16 _ -> U16() 
    | U32 _ -> U32() 
    | U64 _ -> U64() 
    | F32 _ -> F32() 
    | F64 _ -> F64() 
    |> Basic
    |> fun t -> App t []


let rec TypeOfValue v =
    match v with
    | Primitive v -> 
        TypeOfOp v
        |> Ok

    | TypeInstance.Array vs -> 
        List.map TypeOfValue vs
        |> fun ts -> 
            match ts with 
            | [] -> Error "Type Error 3"

            | t :: ts -> 
                List.fold (Result.mapWithError2 Unify) t ts 
                |> Result.map (fun t -> App Array [t])

    | TypeInstance.Struct fields ->
        let fieldsnames, fieldvalues = List.unzip fields
        
        List.map TypeOfValue fieldvalues
        |> List.fold (Result.map2 (fun acc a -> a :: acc)) (Ok[])
        |> Result.map (fun ts -> App (Struct fieldsnames) ts)


let LookUp tab name =
    List.tryFind (fun (id, _) -> name = id) tab
    |> Option.map snd

let i32 = App (Basic (I32())) []
let f32 = App (Basic (F32())) []
let f64 = App (Basic (F64())) []
let unit = App Unit []

let UnifyMany ts ts' =
    List.map2 Unify ts ts'
    |> fun ts -> List.foldBack (Result.map2 (fun t ts -> t :: ts)) ts (Ok[])

let IsFloat t = t = f32 || t = f64 

let IsInt t =
    match t with
    | App(Basic _,_) -> IsFloat t |> not
    | _ -> false


let rec ValidateExpr stab ttab expr =
    match expr with
    | Value(v, info) -> TypeOfValue v
    | Expr.Var(name, info) -> 
        LookUp stab name
        |> Option.toResult $"{name} not defined"
        
    | New(ty, fields) ->
        match ty with
        | App(Array, [t']) ->
            match fields with
            | [idx; initvalue] -> 
                match ValidateExpr stab ttab idx with
                | Ok(App(Basic (I32()),[])) -> 
                    ValidateExpr stab ttab initvalue
                    |> Result.mapWithError (Unify t')
                    |> Result.map (fun t -> App Array [t])
                | Ok ty -> Error $"the index was expected to have type {i32} but got {ty}"
                | err -> err 
            | _ -> Error $"The initiation of an array takes an index and an init value"

        | App(Struct _, fieldTypes) ->
            List.map (ValidateExpr stab ttab) fields
            |> fun ts -> List.foldBack (Result.map2 (fun t ts -> t :: ts)) ts (Ok[])
            |> Result.mapWithError (UnifyMany fieldTypes)
            |> Result.map (fun _ -> ty)

        | _ -> Error "should never happens"

    | IndexOf(id, idx, info) ->
        ValidateExpr stab ttab idx
        |> Result.mapWithError (Unify i32)
        |> Result.mapWithError (fun _ ->
            LookUp stab id
            |> Option.toResult $"the {id} are not defined"
        )

    | FieldOf(id, field, info) ->
        LookUp stab id
        |> Option.toResult $"{id} is not defined"
        |> Result.mapWithError (fun t ->
            match t with
            | App(Struct fields, values) ->
                List.tryFindIndex (fun f -> f = field) fields
                |> Option.map (fun idx -> List.tryItem idx values)
                |> Option.flatten
                |> Option.toResult $"the field {id} are not defined"
            | _ -> Error $"{id} is not a struct"
            )

    | Unary(op, e, info) ->
        ValidateExpr stab ttab e
        |> Result.mapWithError (fun t ->
            match op with
            | Nz | Log | Log10 when IsInt t -> Ok t
            | Round | Nearest 
            | Ceil | Floor | Sqrt when IsFloat t -> Ok t
            | Neg | Abs -> Ok t
            | _ -> Error $"{op.ToString().ToLower()} does not take type {t}"                
        )

    | Binary(op, left, right, info) ->
        (ValidateExpr stab ttab left, ValidateExpr stab ttab right)
        ||> Result.mapWithError2 Unify
        |> Result.mapWithError (fun t ->
            match op with
            | Sl | Sr | Rl | Rr | And | Or | Xor | Imply when IsInt t -> Ok t
            | CopySign when IsFloat t -> Ok t
            | Add | Sub | Mul | Div | Min | Max -> Ok t
            | _ -> Error $"{op.ToString().ToLower()} [{t} {t}] -> {t} does not exist"
        )

    | Compare(op, left, right, info) ->
        (ValidateExpr stab ttab left, ValidateExpr stab ttab right)
        ||> Result.mapWithError2 Unify
        |> Result.map (fun _ -> i32)
     
    | Cvt(ty, e, info) ->
        ValidateExpr stab ttab e
        |> Result.mapWithError (fun t -> 
            match t with
            | App(Basic _,_) -> 
                TypeOfOp ty
                |> Ok
            | _ -> Error $"{t} cannot be converted to {ty}"
        )

let rec ValidateStmt stab ttab stmt =
    match stmt with
    | Assign(id, e, info) ->
        LookUp stab id
        |> Option.toResult $"the {id} are not defined"
        |> Result.mapWithError (fun t ->
            ValidateExpr stab ttab e
            |> Result.mapWithError (Unify t)
            |> Result.map (fun _ -> unit)
        )

    | AssignIndex(id, idx, e, info) ->
        LookUp stab id
        |> Option.toResult $"the {id} are not defined"
        |> Result.mapWithError (fun t ->
            match t with
            | App(Array,[t]) ->
                ValidateExpr stab ttab idx
                |> Result.mapWithError (Unify i32)
                |> Result.mapWithError (fun _ -> 
                    ValidateExpr stab ttab e
                    |> Result.mapWithError (Unify t)
                    |> Result.map (fun _ -> unit)
                )       
            | _ -> Error $"{id} is not an array"
        )

    | AssignField(id, field, e, info) ->
        LookUp stab id
        |> Option.toResult $"the {id} are not defined"
        |> Result.mapWithError (fun t ->
            match t with
            | App(Struct fields, ts) ->
                List.tryFindIndex (fun f -> f = field) fields
                |> Option.map (fun idx -> List.tryItem idx ts)
                |> Option.flatten
                |> Option.toResult $"{id} does not have a field named {field}"
                |> Result.mapWithError2 Unify (ValidateExpr stab ttab e)
                |> Result.map (fun _ -> unit)
                )
    
    | If(cond, tbranch, fbranch, info) ->
        ValidateExpr stab ttab cond
        |> Result.mapWithError (Unify i32)
        |> Result.mapWithError (fun _ ->
            (ValidateSeq stab ttab tbranch, ValidateSeq stab ttab fbranch)
            ||> Result.mapWithError2 Unify
        )
        
    | When(cond, branch, info) ->
        ValidateExpr stab ttab cond
        |> Result.mapWithError (Unify i32)
        |> Result.mapWithError (fun _ ->
            ValidateSeq stab ttab branch
        )

    | While(cond, loop, info) ->
        ValidateExpr stab ttab cond
        |> Result.mapWithError (Unify i32)
        |> Result.mapWithError (fun _ ->
            (Ok unit, List.map (ValidateStmt stab ttab) loop)
            ||> List.fold (Result.mapWithError2 Unify)
        )

    | Return(e, info) -> ValidateExpr stab ttab e


and ValidateSeq stab ttab s =
    let rec loop u s =
        match s with 
        | [] -> Ok u
        | s :: ss -> 
            ValidateExpr stab ttab s
            |> Result.mapWithError (fun t ->
                Unify u unit
                |> Result.map(fun _ -> t)
            )
            |> Result.mapWithError (fun t -> loop t ss)

    loop unit s 


let ValidateData data = TypeOfValue data.Content
    
