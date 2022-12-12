module Type


type Result<'s,'e> with
    static member map2 f ret1 ret2 = 
        match ret1, ret2 with
        | Error msg, _ | _, Error msg -> Error msg
        | Ok ret1, Ok ret2 -> Ok(f ret1 ret2)

    static member mapWithError f ret =
        match ret with
        | Ok ret -> f ret
        | err -> err

    static member mapWithError2 f ret1 ret2 =
        match ret1, ret2 with
        | Error msg, _ | _, Error msg -> Error msg
        | Ok ret1, Ok ret2 -> f ret1 ret2


type Operand<'i8,'i16,'i32,'i64,'u8,'u16,'u32,'u64,'f32,'f64> =
    | I8  of 'i8
    | I16 of 'i16
    | I32 of 'i32
    | I64 of 'i64
    | U8  of 'u8
    | U16 of 'u16
    | U32 of 'u32
    | U64 of 'u64
    | F32 of 'f32
    | F64 of 'f64

    
type ty = Operand<unit, unit, unit, unit, unit, unit, unit, unit, unit, unit>


type Type<'id> =
    | Nul
    | Var of 'id
    | App of TyCon<'id> * Type<'id> list
    | Poly of 'id list * Type<'id>

and TyCon<'id> =
    | Unit
    | Arrow
    | Array
    | Basic of ty
    | Struct of 'id list
    | TyFun of 'id list * Type<'id>
    | Unique of TyCon<'id> * uint64


let mutable private t = 0

let TyVar _ = 
    let ret = t
    t <- t + 1
    $"tyvar {ret}"
    |> Var

let App tycon ts = App(tycon, ts)

let Poly ts t = Poly(ts, t)

let TyFun tyvars t = TyFun(tyvars, t)

let rec Subst ty sigma =
    match ty with
    | App(TyFun(alphas, t), tyvars) ->
        List.zip alphas tyvars @ sigma
        |> Subst t

    | App(tycon, ts) ->
        List.map (fun t -> Subst t sigma) ts
        |> App tycon
    
    | Poly(tyvars, t) -> 
        List.map TyVar tyvars
        |> List.zip tyvars
        |> Subst t
        |> fun t -> Subst t sigma

    | Var id -> 
        defaultArg 
            (
                List.tryFind (fun (id', _) -> id' = id) sigma 
                |> Option.map snd
            ) 
            ty

    | Nul -> ty


let rec Unify t1 t2 =
    match t1, t2 with
    | App(TyFun(alphas, u),ts), t
    | t, App(TyFun(alphas, u), ts) ->
        List.zip alphas ts
        |> Subst u
        |> Unify t

    | App(tycon, ts), App(tycon', ts') when tycon = tycon' ->
        List.map2 Unify ts ts'
        |> fun ts -> List.foldBack (Result.map2 (fun t ts -> t :: ts)) ts (Ok[])
        |> Result.map (App tycon)
        
    | App(Unique(tycon, z), ts), App(Unique(u', z'), ts') ->
        if z <> z' then
            Error $"Type Error 1"
        else
            List.map2 Unify ts ts'
            |> fun ts -> List.foldBack (Result.map2 (fun t ts -> t :: ts)) ts (Ok[])
            |> Result.map (App tycon)

    | Poly(alphas, u), Poly(alphas', u') ->
        List.map Var alphas'
        |> List.zip alphas
        |> Subst u'
        |> Unify u

    | Var a, Var b when a = b -> Ok t1
    
    | Nul, (App(Struct _, _) as t)
    | (App(Struct _, _) as t), Nul -> Ok t

    | _ -> Error $"Type Error 2"


let rec Expand t =
    match t with
    | App(TyFun(alphas, u), ts) ->
        List.zip alphas ts
        |> Subst u
        |> Expand

    | App(Unique(tycon, z), ts) ->
        App tycon ts
        |> Expand

    | _ -> t