namespace AbstractSyntax

module Funky =

    // a generic tagged type used both for values and typing
    type op<'b, 'i8,'i16,'i32,'i64,'u8,'u16,'u32,'u64,'f32,'f64,'arr, 'record> =
        | Zero
        | One
        | Bool      of 'b
        | I8        of 'i8
        | I16       of 'i16
        | I32       of 'i32
        | I64       of 'i64
        | U8        of 'u8
        | U16       of 'u16
        | U32       of 'u32
        | U64       of 'u64
        | F32       of 'f32
        | F64       of 'f64
        | Array     of 'arr list
        | Record    of (string * 'record) list

        

    // tag needed because of type error
    type Value = 
        | V of op<int, int8, int16, int, int64, uint8, uint16, uint, uint64, float32, float, Value, Value>
        

    // tag needed because of type error
    type Type = T of op<unit, unit, unit, unit, unit, unit, unit, unit, unit, unit, unit, Type, Type>     
   
    type FuncType = F of Type Set list * Type

    type Position = 
        {
            line: uint
            offset: uint
            absolute: uint
        }
    with
        static member start = { line = 0u; offset = 0u; absolute = 0u}
        
        override p.ToString() = $"({p.line}, {p.offset})"


    let Move n pos = { pos with offset = pos.offset + n; absolute = pos.absolute + n }
    
    let Next pos = Move 1u pos

    let NewLine pos = { line = pos.line + 1u; offset = 0u; absolute = pos.absolute + 1u}

    // this should be extended 
    type Info<'Type when 'Type:comparison> =
        {
            pos: Position
            ty: 'Type Set
        }


    type Expr =
        | Val        of Value  * Type Info
        | Var        of string * Type Info
        | Declare    of string * Expr * Expr * Type Info 
        | If         of Expr * Expr * Expr * Type Info
        | Neg        of Expr * Type Info
        | Not        of Expr * Type Info
        | Add        of Expr * Expr * Type Info
        | Sub        of Expr * Expr * Type Info
        | Mul        of Expr * Expr * Type Info
        | Div        of Expr * Expr * Type Info
        | Rem        of Expr * Expr * Type Info
        | LeftShift  of Expr * Expr * Type Info
        | RightShift of Expr * Expr * Type Info
        | And        of Expr * Expr * Type Info
        | Or         of Expr * Expr * Type Info
        | Xor        of Expr * Expr * Type Info
        | Imply      of Expr * Expr * Type Info
        | Greater    of Expr * Expr * Type Info
        | Geq        of Expr * Expr * Type Info
        | Eq         of Expr * Expr * Type Info
        | Nq         of Expr * Expr * Type Info
        | Leq        of Expr * Expr * Type Info
        | Less       of Expr * Expr * Type Info
        | Call       of string * Expr list * FuncType Info
        

    type Declare = 
        | Func of name: string * argsnames: string list * body: Expr * FuncType Info
        | Import of string list * unit Info
        | Type of string * Type * unit Info

    type Program = Program of Declare list * main: Expr option


    let CompileError (case: string) (msg: string) (pos: Position) = Error $"{case}:\n  {msg} {pos}\n"