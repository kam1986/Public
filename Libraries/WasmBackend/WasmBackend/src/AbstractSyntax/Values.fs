


module Values 
#nowarn "25"
open Types

let typeOfNum op : numType = 
    match op with
    | I32 _ -> I32()
    | I64 _ -> I64()
    | U32 _ -> U32()
    | U64 _ -> U64()
    | F32 _ -> F32()
    | F64 _ -> F64()
    
type num = op<int, int64, uint, uint64, float32, float>

let ppNum (n: num) =
    match n with
    | I32 n -> string n
    | U32 n -> string n
    | I64 n -> string n
    | U64 n -> string n
    | F32 f -> string f
    | F64 f -> string f
    

type vec128 = Vec128<int[], int64[], uint[], uint64[], float32[], float[]>


type 'id ref_ = Ref of 'id | FuncRef of 'id | Null of refType 
   

let ppRef ref =
    match ref with
    | Ref _ -> "ref"
    | _     -> "null"


type 'id value = 
    | Num of num 
    | Ref of 'id ref_ 

let ppValue v =
    match v with
    | Num v -> ppNum v
    | Ref r -> ppRef r


let stringOfValues : 'id value list -> string = function
    | [v] -> string v
    | vs -> "[" + String.concat " " (List.map ppValue vs) + "]"