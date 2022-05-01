module Types 


// This are reused heavily
// the extention of types are because it is used in
// as an vector type instant and when rewriten also as load/store memop
// sign extention are for operator handling
type op< 'i32, 'i64, 'u32, 'u64, 'f32, 'f64> =
    | I8  of 'i32
    | I16 of 'i32
    | U8  of 'u32
    | U16 of 'u32
    | I32 of 'i32 
    | I64 of 'i64
    | U32 of 'u32   
    | U64 of 'u64   
    | F32 of 'f32 
    | F64 of 'f64  

type sectionID =
    | Custom    = 0  
    | Type      = 1
    | Import    = 2
    | Function  = 3
    | Table     = 4
    | Memory    = 5
    | Global    = 6
    | Export    = 7
    | Start     = 8
    | Element   = 9 
    | Code      = 10
    | Data      = 11
    | Datacount = 12


type Vec128< 'i32, 'i64, 'u32, 'u64, 'f32, 'f64> = 
    Vec128 of op< 'i32, 'i64, 'u32, 'u64, 'f32, 'f64>

type numType = op<unit, unit, unit, unit, unit, unit> 
        
type vecType = Vec128<unit, unit, unit, unit, unit, unit>


let ppNumType (t: numType) =
        match t with
        | I8 _  | U8 _
        | I16 _ | U16 _
        | I32 _ | U32 _ -> "i32"
        | I64 _ | U64 _ -> "i64"
        | F32 _ -> "f32"
        | F64 _ -> "f64"


type refType = FuncRef | ExternRef 

let ppRefType ref =
    match ref with
    | FuncRef -> "funcref"
    | ExternRef -> "externref"

type RefInstance = 
    | Null of refType
    | Ref of refType

let stringOfRefedType = function
    | FuncRef -> "func"
    | ExternRef -> "extern"



        

type valueType = NumType of numType | RefType of refType | VecType of vecType

let ppValueType value =
    match value with
    | RefType t -> ppRefType t // string calls the ToString member of the type instance t
    | NumType t -> ppNumType t
    | _ -> ""

// empty case are handled in the last case
let stringOfValueTypes = function
    | [t] -> ppValueType t
    | ts -> "[" + String.concat " " (List.map ppValueType ts) + "]"

type resultType = Result of valueType list

let ppResultType result =
    let (Result ret) = result
    let s = String.concat " " (List.map ppValueType ret)
    $"[{s}]"



type funcType = FuncType of resultType * resultType 

let ppFunctionType func =
    let(FuncType(input, output)) = func
    $"{input} -> {output}"


type share = Unshared | Shared


type limits = { min: uint; max: uint option; share: share } 

let ppLimit limit =
    string limit.min +
    match limit.max with
    | None -> ""
    | Some n -> " " + string n
    + (string limit.share).ToLower()

type mutability = Immutable | Mutable


type tableType = TableType of limits * refType

let ppTableType tt =
    let (TableType(lim, t)) = tt
    string lim + " " + string t

type memoryType = MemoryType of limits 

let ppMemoryType mem = 
    let (MemoryType lim) = mem
    string lim

type globalType = GlobalType of valueType * mutability 

let ppGlobalType glob =
    let (GlobalType(t,mut)) = glob
    match mut with
    | Immutable -> string t
    | _ -> "(mut " + string t + ")"

type externType =
    | ExternFuncType of funcType
    | ExternTableType of tableType
    | ExternMemoryType of memoryType
    | ExternGlobalType of globalType
with
    override ext.ToString() =
        match ext with
        | ExternFuncType ft -> "func " + ppFunctionType ft
        | ExternTableType tt -> "table " + ppTableType tt
        | ExternMemoryType mt -> "memory " + ppMemoryType mt        
        | ExternGlobalType gt -> "global " + ppGlobalType gt
        

type packSize = Pack8 | Pack16 | Pack32 
type extension = SX  | ZX


let size = function
    | (I64 _) | (U64 _) | (F64 _) -> 8
    | _ -> 4




