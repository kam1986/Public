namespace AbstractSyntax 
module C =
    open System.Text

    type 'id Active = 
        | Active of 'id 
        | Passive of 'id


    type COp<'i8,'i16,'i32,'i64,'u8,'u16,'u32,'u64,'f32,'f64> =
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
        | Tuple     of COp<'i8,'i16,'i32,'i64,'u8,'u16,'u32,'u64,'f32,'f64> list
        | Unit
    
    // used to 
    type IDs<'id> = COp<'id, 'id, 'id, 'id, 'id, 'id, 'id, 'id, 'id, 'id> list
    
    type Value = 
        COp<
            int8,
            int16,
            int32,
            int64,
            uint8,
            uint16,
            uint32,
            uint64,
            float32,
            float
          > 

    
    type Data = 
        | Str  of string
        | I8s  of int8 []
        | I16s of int16 []
        | I32s of int32 []
        | I64s of int64 []
        | U8s  of uint8 []
        | U16s of uint16 []
        | U32s of uint32 []
        | U64s of uint64 []
        | F32s of float32 []
        | F64s of float []
    with 
        static member ToBytes data =   
            match data with
            | Str data -> Encoding.UTF8.GetBytes data
            | I8s data -> Array.map byte data
            | U8s data -> data
            | I16s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: int16)) data
                |> Array.concat

            | I32s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: int32)) data
                |> Array.concat
            
            | I64s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: int64)) data
                |> Array.concat

            | U16s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: uint16)) data
                |> Array.concat

            | U32s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: uint32)) data
                |> Array.concat


            | U64s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: uint64)) data
                |> Array.concat

            | F32s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: float32)) data
                |> Array.concat
            
            | F64s data -> 
                Array.map (fun i -> System.BitConverter.GetBytes(i: float)) data
                |> Array.concat
                

    


    and Type =
        COp<
            unit,
            unit,
            unit,
            unit,
            unit,
            unit,
            unit,
            unit,
            unit,
            unit
        >
    
    and Cvt<'id,'fid, 'Type> = 
        COp<
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>,
            Expr<'id,'fid, 'Type>
        >


    and Types = Type list


    and unOp =  Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt 

    and binOp = 
        | Add  | Sub | Div | Mul | LeftShift | RightShift | LeftRotate | RightRotate | And | Or | Xor | Rem
        | Rotl | Rotr
        | Min  | Max | CopySign 
  
  
    and relOp = Eq | Nq | Greater | Geq | Less | Leq 


    and Index<'id,'fid, 'Type> = Index of 'id * Expr<'id,'fid, 'Type>

    and Expr<'id,'fid, 'Type> =
        | True 
        | False
        | Val       of Value
        | Convert   of Cvt<'id,'fid, 'Type>
        | Var       of 'id
        | Deference of 'Type * Expr<'id,'fid, 'Type>
        | Binop     of 'Type * binOp * Expr<'id,'fid, 'Type> * Expr<'id,'fid, 'Type>
        | Relop     of 'Type * relOp * Expr<'id,'fid, 'Type> * Expr<'id,'fid, 'Type>  
        | Unop      of 'Type * unOp * Expr<'id,'fid, 'Type> 
        | Call      of 'Type * 'fid * Expr<'id,'fid, 'Type> list 
        | If        of 'Type * Expr<'id,'fid, 'Type> * Expr<'id,'fid, 'Type> * Expr<'id,'fid, 'Type>
        //| MemSize 



    and Statement<'id, 'fid, 'Type> =
        | Dec       of 'Type * 'id * Expr<'id,'fid, 'Type> 
        | Assign    of 'id * Expr<'id,'fid, 'Type>
        | If        of Expr<'id,'fid, 'Type> * Statement<'id,'fid, 'Type> * Statement<'id,'fid, 'Type> option 
        | While     of Expr<'id,'fid, 'Type> * Statement<'id,'fid, 'Type> * Type option
        | Sequence  of Statement<'id,'fid, 'Type> * Statement<'id,'fid, 'Type>
        | Expr      of Expr<'id,'fid, 'Type>
        | Return    
        | Break
        | Continue


    and Function<'id,'fid, 'Type> = Fun of 'fid * IDs<'id> * 'Type * Statement<'id,'fid, 'Type> 


    and Functions<'id,'fid, 'Type> = Funs of Function<'id,'fid, 'Type> list

    and Global<'id,'fid, 'Type> = Global of ('id * 'Type * Expr<'id,'fid, 'Type>)

    and Globals<'id,'fid, 'Type> = Globals of Global<'id,'fid, 'Type> list


    and Program<'id,'fid, 'Type> = 
        Enviroment of 
            Globals<'id,'fid, 'Type>     // global variables not acceable to the programmer
            * ('id * Data) list             // staticly known data at compile time
            * Functions<'id,'fid, 'Type>  // the collection of all functions in the program.

