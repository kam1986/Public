namespace Wasm

#nowarn "25"
open Types

module Values =

    type op<'i32, 'i64, 'u32, 'u64, 'f32, 'f64> = 
        | I32 of 'i32 
        | I64 of 'i64
        | U32 of 'u32   // for simplicity 
        | U64 of 'u64   // for simplicity 
        | F32 of 'f32 
        | F64 of 'f64 with
        override op.ToString() =
            match op with
            | I32 n -> string n
            | I64 n -> string n
            | F32 f -> string f
            | F64 f -> string f

    let typeOfNum = function
    | I32 _ -> I32_t
    | I64 _ -> I64_t
    | U32 _ -> I32_t
    | U64 _ -> I64_t
    | F32 _ -> F32_t
    | F64 _ -> F64_t
    
    type num = op<int, int64, uint, uint64, float32, float>


    type 'id ref_ = Ref of 'id | FuncRef of 'id | Null of refType with
        override ref.ToString() =
            match ref with
            | Ref _ -> "ref"
            | _   -> "null"

    type 'id value = 
        | Num of num 
        | Ref of 'id ref_ 


    let as_num = function
    | Num n -> n
    | Ref _ -> failwith "as_num"


    let as_ref = function
    | Num _ -> failwith "as_ref"
    | Ref r -> r


    let default_num = function
        | I32_t -> I32 0
        | I64_t -> I64 0L
        | F32_t -> F32 0.f
        | F64_t -> F64 0.


    let default_ref = function
        | t -> Null t


    let default_value = function
    | NumType t' -> Num (default_num t')
    | RefType t' -> Ref (default_ref t')


    let value_of_bool b = Num (I32 (if b then 1 else 0))


    // let stringOfRef' = ref (function Null _ -> "null" | _ -> "ref")
    // let stringOfRef  = !stringOfRef'


    let stringOfValue = function
        | Num n -> string n
        | Ref r -> string r


    let stringOfValues : 'id value list -> string = function
        | [v] -> string v
        | vs -> "[" + String.concat " " (List.map string vs) + "]"