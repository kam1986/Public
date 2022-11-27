module Syntax

open Position
open Token
open Unique

#nowarn "62"


type location =
    | Stack
    | Heap

type 'ty Info =
    {
        pos: Position
        ty: 'ty                // type of the value
    }

let info token =
    {
        ty         = ()
        pos        = PosOf token
    }

// used in symboltables
type ('v, 'ty) value =
    {
        value: 'v
        sideEffect: bool // to be set if the variable has some kind of side effect
        unique: bool  
        shared: bool
        iteratable: bool
        owner: string
        borrowers: uint
        location: location
        reversed: bool option
    }

let initvalue v = 
    {
        value      = v
        sideEffect = false
        unique     = false
        shared     = false
        iteratable = false
        owner      = ""
        borrowers  = 0u
        location   = Stack
        reversed   = None
    }


// should be extended later on
type ('b, 'i, 'u, 'f) op =
    | Bool  of 'b
    | I8    of 'i
    | I16   of 'i
    | I32   of 'i
    | I64   of 'i
    | U8    of 'u
    | U16   of 'u
    | U32   of 'u
    | U64   of 'u
    | F32   of 'f
    | F64   of 'f
with
    override op.ToString() =
        match op with
        | Bool b -> string b
        | I8  i   
        | I16 i 
        | I32 i 
        | I64 i -> string i
        | U8  i 
        | U16 i 
        | U32 i 
        | U64 i -> string i
        | F32 f
        | F64 f -> string f

    static member Arg op =
        match op with
        | Bool e -> e
        | I8   e -> e
        | I16  e -> e
        | I32  e -> e
        | I64  e -> e
        | U8   e -> e
        | U16  e -> e
        | U32  e -> e
        | U64  e -> e
        | F32  e -> e
        | F64  e -> e


    static member map f op =
        match op with
        | Bool e -> Bool (f e)
        | I8   e -> I8   (f e)
        | I16  e -> I16  (f e)
        | I32  e -> I32  (f e)
        | I64  e -> I64  (f e)
        | U8   e -> U8   (f e)
        | U16  e -> U16  (f e)
        | U32  e -> U32  (f e)
        | U64  e -> U64  (f e)
        | F32  e -> F32  (f e)
        | F64  e -> F64  (f e)

type Value = (bool, int64, uint64, float) op

type Type = (unit, unit, unit, unit) op

let rec pptype = function
    | Bool _ -> "bool"
    | I8  _  -> "i8"
    | I16 _  -> "i16"
    | I32 _  -> "i32"
    | I64 _  -> "i64"
    | U8  _  -> "u8"
    | U16 _  -> "u16"
    | U32 _  -> "u32"
    | U64 _  -> "u64"
    | F32 _  -> "f32"
    | F64 _  -> "f64"
  

type unop = 
    | Neg 
    | Not 
    | Pc 
    | Clz 
    | Ctz 
    | Sqrt 
    | Round 
    | Nearest 
    | Floor 
    | Ceil 
    | Abs
with 
    override u.ToString() =
        match u with
        | Neg     -> "neg"
        | Not     -> "not "
        | Pc      -> "popcnt "
        | Clz     -> "clz "
        | Ctz     -> "ctz "
        | Sqrt    -> "sqrt "
        | Floor   -> "floor "
        | Ceil    -> "ceil "
        | Abs     -> "abs "
        | Nearest -> "nearest "
        | Round   -> "round "


type binop =
    | Add 
    | Sub 
    | Mul 
    | Div 
    | Rshft
    | LShft
with
    override b.ToString() =
        match b with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | LShft -> "<<"
        | Rshft -> ">>"

type relop = Eq | Nq | Le | Lt | Ge | Gt

(*

    need to implement unzip in generalized form for tuples and array
    idea for tuple 
        unzip: k0 * .. * ki -> t0 * .. * tj -> (t0 * .. * t{k0})*(t{k0+1} * .. * t{k1}) * (t{k{i-1}+1} * .. * tj)  
        this should work as long as we limit the first tuple to type restriction kj : i32 for all 0 <= j <= i.
        and the sum over k's are the number of fields in the tuple
*)

type 'ty Expr =
    | Value  of Value * 'ty Info
    | Tuple  of 'ty Expr [] * 'ty Info                      // defined here to allow nested expression
    | Array  of 'ty Expr [] * 'ty Info                      // defined here to allow nested expression
    //| Init   of 'ty Expr * 'ty Info                         // init an array of size determined by the expression
    | Zip    of 'ty Expr[] * 'ty Info                       // zip n iteratable collections together (it will treat it as an iterater of n dimensional tuples) it is allowed to zip n tuples together resulting in a new tuple.
    | Map    of 'ty Expr * 'ty Expr * 'ty Info              // map    an iteratable collection
    | Filter of 'ty Expr * 'ty Expr * 'ty Info              // filter an iteratable collection
    | Fold   of 'ty Expr * 'ty Expr * 'ty Expr * 'ty Info   // fold   an iteratable collection
    | Scan   of 'ty Expr * 'ty Expr * 'ty Expr * 'ty Info   // scan   an iteratable collection
    | Rev    of 'ty Expr * 'ty Info                         // check if the result of the expression has a rev function on it and set the reverse field. an iteratable type as a default rev function
    | Until  of 'ty Expr * 'ty Expr * 'ty Info
    | From   of 'ty Expr * 'ty Expr * 'ty Info
    | Variable of string * 'ty Info 
    | Unary of unop * 'ty Expr *  'ty Info
    | If of 'ty Expr * 'ty Expr * 'ty Expr * 'ty Info
    | Binary of binop * 'ty Expr * 'ty Expr * 'ty Info
    | Convert of ('ty Expr, 'ty Expr, 'ty Expr,  'ty Expr) op * 'ty Info
    // we use expressions build up as variables, array, tuple or struct notation
    | Declare of 'ty Expr[] * 'ty Expr[] * 'ty Info
    | Fun of string * string[] * 'ty Expr * 'ty Info
    | Call of string * 'ty Expr[] * 'ty Info
    // This simplifies passing result from on calcualtion into another without the need to define variables
    // It also allows for expressions with no return values
    | Continuation of 'ty Expr * 'ty Expr * 'ty Info
with 
    override e.ToString() =
        match e with
        | Variable(v,_) -> $"{v}"
        | Value(v, _) -> $"{v}"
        | Tuple(tp, _) -> "(" + String.concat ", " (Array.map string tp) + ")"
        | Array(arr, _) ->  "[" + String.concat ", " (Array.map string arr) + "]"
        | Call(name, args, _) -> 
            let nested arg =
                match arg with
                | Value _ 
                | Tuple _
                | Array _ -> false
                | _ -> true

            Array.map (fun arg -> if nested arg then $"({arg})" else string arg) args
            |> String.concat " "
            |> (+) (name + " ")
           
        | Zip(iters, _) -> 
            let iters = Array.map string iters |> String.concat " "
            $"zip {iters}"

        | Map(f, expr, _)       -> $"map {f} {expr}"
        | Filter(f, expr, _)    -> $"filter {f} {expr}"
        | Fold(f, acc, expr, _) -> $"fold {f} {acc} {expr}"
        | Scan(f, acc, expr, _) -> $"scan {f} {acc} {expr}" 
        | From(f, expr, _)       -> $"from {f} {expr}"
        | Until(f, expr, _)       -> $"until {f} {expr}"
        | Continuation(first, second, _) -> $"{first}; {second}"
        | Declare(names, bodies, _) -> 
            let names = String.concat ", " <| Array.map string names
            let bodies = Array.map string bodies |> String.concat ", "
            $"let {names} = {bodies}"
        | If(cond, t, f, _) ->
            $"if {cond} then {t} else {f}"
        
        | Init(n, info)   -> $"{info.ty}[{n}]"
        | Convert(cvt, _) -> $"{pptype cvt} ({cvt})"
        | Rev(iter, _) -> 
            match iter with
            | Array _   -> $"rev {iter}"
            | _         -> $"rev ({iter})"

        | Fun(name, ids, body, _) -> 
            let ids = String.concat " " ids
            $"fun {name} {ids} = {body}"

        | Unary(op, (Value _ as e), _) -> $"{op}{e}"
        | Unary(op, e, _) ->  $"{op}({e})"
        | Binary(op, left, right, _) when op = Mul || op = Div ->
            let left =
                match right with
                | Binary(op, _, _, _) when op = Add || op = Sub -> $"({left})"
                | _ -> $"{left}"

            let right =
                match right with
                | Binary(op, _, _,_) when op = Add || op = Sub -> $"({right})"
                | _ -> $"{right}"  

            $"{left} {op} {right}"

        | Binary(op, left, right, _) -> $"{left} {op} {right}"

        | Convert(e, _) ->
            match e with
            | Bool (Value _) as e ->  $"bool {e}"
            | I8  (Value _)  as e ->  $"i8 {e}"
            | I16 (Value _)  as e ->  $"i16 {e}"
            | I32 (Value _)  as e ->  $"i32 {e}"
            | I64 (Value _)  as e ->  $"i64 {e}"
            | U8  (Value _)  as e ->  $"u8 {e}"
            | U16 (Value _)  as e ->  $"u16 {e}"
            | U32 (Value _)  as e ->  $"u32 {e}"
            | U64 (Value _)  as e ->  $"u64 {e}"
            | F32 (Value _)  as e ->  $"f32 {e}"
            | F64 (Value _)  as e ->  $"f64 {e}"
            | Bool e ->  $"bool ({e})"
            | I8  e  ->  $"i8 ({e})"
            | I16 e  ->  $"i16 ({e})"
            | I32 e  ->  $"i32 ({e})"
            | I64 e  ->  $"i64 ({e})"
            | U8  e  ->  $"u8 ({e})"
            | U16 e  ->  $"u16 ({e})"
            | U32 e  ->  $"u32 ({e})"
            | U64 e  ->  $"u64 ({e})"
            | F32 e  ->  $"f32 ({e})"
            | F64 e  ->  $"f64 ({e})"
            
            
let getinfo e =
    match e with
    | Value(_, i)
    | Tuple(_, i)  
    | Array(_, i)  
    | Init(_, i)   
    | Zip(_, i)    
    | Map(_, _, i)    
    | Filter(_, _, i)
    | Fold(_, _, _, i)
    | Scan(_, _, _, i)
    | Rev(_, i)
    | Until(_, _, i)
    | From(_, _, i)  
    | Variable(_, i)
    | Unary(_, _, i)
    | If(_, _, _, i)
    | Binary(_, _, _, i)
    | Convert(_, i)
    | Declare(_, _, i)
    | Fun(_, _, _, i)
    | Call(_, _, i)
    | Continuation(_, _, i) -> i 

type 'ty Type =
    | Struct of typename: string * fields: (string * 'ty)[]
    

type 'ty Program = Program of 'ty Type[] * 'ty Expr[]
    