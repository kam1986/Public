namespace CLI

module Type =
   

    type Numerical<'i8, 'i16, 'i32,'i64, 'u8, 'u16, 'u32, 'u64, 'native, 'F> =
        | I8        of 'i8
        | I16       of 'i16
        | I32       of 'i32
        | I64       of 'i64
        | U8        of 'u8
        | U16       of 'u16
        | U32       of 'u32
        | U64       of 'u64
        | Native    of 'native
        | F         of 'F


    type Object<'o> =
        | Undefined of 'o


    type Pointer<'item> =
         | Unmanaged of 'item
         | Managed   of 'item
         | Null


    type Type<'i8, 'i16, 'i32,'i64, 'u8, 'u16, 'u32, 'u64, 'native, 'F, 'o, 'item> =
        | Num of Numerical<'i8, 'i16, 'i32,'i64, 'u8, 'u16, 'u32, 'u64, 'native, 'F>
        | Obj of Object<'o>
        | Poiter of Pointer<'item>


    module IntOp =
        
        type binop = 
            Undefined

        type unop =
            Undefined


    module FloatOp =
        
        type binop = 
            Undefined

        type unop =
            Undefined


    module NativeOp =
        
        type binop = 
            Undefined

        type unop =
            Undefined


    type binop = Numerical<IntOp.binop, IntOp.binop, IntOp.binop, IntOp.binop, IntOp.binop, IntOp.binop, IntOp.binop, IntOp.binop, FloatOp.binop, NativeOp.binop>
    type unop = Numerical<IntOp.unop, IntOp.unop, IntOp.unop, IntOp.unop, IntOp.unop, IntOp.unop, IntOp.unop, IntOp.unop, FloatOp.unop, NativeOp.unop>


    type Instruction =
        | BinOp of binop
        | UnOp of unop