namespace Wasm

#nowarn "25"    
module Types = 

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

    type numType = I32_t | I64_t | F32_t | F64_t with 
        override num.ToString() = 
            match num with
            | I32_t -> "i32"
            | I64_t -> "i64"
            | F32_t -> "f32"
            | F64_t -> "f64"

    type refType = FuncRef | ExternRef with
        override ref.ToString() =
            match ref with
            | FuncRef -> "funcref"
            | ExternRef -> "externref"

    type RefInstance =
        | Null of refType
        | Ref of refType

    let stringOfRefedType = function
        | FuncRef -> "func"
        | ExternRef -> "extern"


    type valueType = NumType of numType | RefType of refType with
        override value.ToString() =
            match value with
            | RefType t -> string t // string calls the ToString member of the type instance t
            | NumType t -> string t

    // empty case are handled in the last case
    let stringOfValueTypes : valueType list -> string = function
        | [t] -> string t
        | ts -> "[" + String.concat " " (List.map string ts) + "]"

    type resultType = Result of valueType list with
        override result.ToString() =
            let (Result ret) = result
            let s = String.concat " " (List.map string ret)
            $"[{s}]"



    type funcType = FuncType of resultType * resultType with
        override func.ToString() =
            let(FuncType(input, output)) = func
            $"{input} -> {output}"


    type share = Unshared | Shared

    type limits = { min: int; max: int option; share: share } with
        override limits.ToString() =
            string limits.min +
            match limits.max with
            | None -> ""
            | Some n -> " " + string n
            + (string limits.share).ToLower()

    type mutability = Immutable | Mutable

    let mut b =
        match b with
        | 0x00uy -> Immutable
        | 0x01uy -> Mutable

    type tableType = TableType of limits * refType  with
        override table.ToString() =
            let (TableType(lim, t)) = table
            string lim + " " + string t

    type memoryType = MemoryType of limits with
        override memory.ToString() =
            let (MemoryType lim) = memory
            string lim

    type globalType = GlobalType of valueType * mutability with
        override glob.ToString() =
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
            | ExternFuncType ft -> "func " + string ft
            | ExternTableType tt -> "table " + string tt
            | ExternMemoryType mt -> "memory " + string mt        
            | ExternGlobalType gt -> "global " + string gt
        

    type packSize = Pack8 | Pack16 | Pack32 
    type extension = SX  | ZX


    let size = function
        | I32_t | F32_t -> 4
        | I64_t | F64_t -> 8


    let isNum = function
        | NumType _ -> true
        | RefType _ -> false

    let IsRef = function
        | RefType _ -> true
        | NumType _ -> false


    let funcs = 
        List.map (fun t -> match t with ExternFuncType t -> Some t  | _ -> None)

    let tables =
        List.map (fun t -> match t with ExternTableType t -> Some t  | _ -> None)

    let memories =
        List.map (fun t -> match t with ExternMemoryType t -> Some t  | _ -> None)

    let globals =
        List.map (fun t -> match t with ExternGlobalType t -> Some t  | _ -> None)

    let matchLimits lim1 lim2 =
        lim1.min >= lim2.min &&
        match lim1.max, lim2.max with
        | _, None -> true
        | None, Some _ -> false
        | Some i, Some j ->  i <=  j

    let matchFuncType ft1 ft2 = 
        ft1 = ft2

    let matchTableType (TableType (lim1, et1)) (TableType (lim2, et2)) =
        et1 = et2 && matchLimits lim1 lim2

    let matchMemoryType (MemoryType lim1) (MemoryType lim2) = matchLimits lim1 lim2

    let matchGlobalType gt1 gt2 =
        gt1 = gt2

    let matchExternType et1 et2 =
        match et1, et2 with
        | ExternFuncType ft1, ExternFuncType ft2 -> matchFuncType ft1 ft2
        | ExternTableType tt1, ExternTableType tt2 -> matchTableType tt1 tt2
        | ExternMemoryType mt1, ExternMemoryType mt2 -> matchMemoryType mt1 mt2
        | ExternGlobalType gt1, ExternGlobalType gt2 -> matchGlobalType gt1 gt2
        | _, _ -> false

