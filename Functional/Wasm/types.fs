namespace Wasm

module Types =
    
    // abbriviations of the types
    // to better interop with the specifications
    type i8     = int8
    type i16    = int16
    type i32    = int
    type i64    = int64

    type u8     = uint8
    type u16    = uint16
    type u32    = uint
    type u64    = uint64

    type f32    = float32
    type f64    = float 

    // this is souly for the ease of changing type
    // and readability
    type typeidx    = u32
    type funcidx    = u32
    type tableidx   = u32
    type memidx     = u32
    type globalidx  = u32
    type elemidx    = u32
    type dataidx    = u32
    type localidx   = u32
    type labelidx   = u32


    type memarg     = unativeint * unativeint


    // reference types
    // these shall be stored in tables
    type RefType = FuncRef | ExternRef with
        override ref.ToString() =
            match ref with
            | FuncRef   -> "funcref"
            | ExternRef -> "externref"

    // value types
    type NumType = 
        | I8  | I16 | I32 | I64 
        | F32 | F64 
    with
        override nt.ToString() =
            match nt with
            | I8    -> "i8"
            | I16   -> "i16"
            | I32   -> "i32"
            | I64   -> "i64"
            | F32   -> "f32"
            | F64   -> "f64"

    // a union of both types
    type ValType = NumType of NumType | RefType of RefType with
        override v.ToString() =
            match v with
            | NumType t -> t.ToString()
            | RefType t -> t.ToString()

    // return type
    // using sequence both because of lazy evaluation
    type ResultType = ValType list

    // function type ResultType -> ResultType
    type FuncType = Func of (ResultType * ResultType)

    // not clearly specified if this should be an interval
    type Limits = LowerBound of unativeint | Interval of unativeint * unativeint
   
    // This is a wrapper type.
    // This 
    type 'a vec = 'a list
  


    let vec v = List.ofSeq (seq v) : _ vec

    type MemType = Limits of Limits

    type TableType = Table of Limits * RefType

    type Mut = Const | Mut

    type GlobalType = GlobalType of Mut * ValType

    type ExternType = 
        | ExFunc of FuncType 
        | ExTable of TableType 
        | ExMemType of MemType 
        | ExGlobal of GlobalType
    
    type BlockType = TBlock of typeidx | VBlock of ValType | NoType


    type types = FuncType vec


