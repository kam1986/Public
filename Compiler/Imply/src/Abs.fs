module Abs

open Type



type UnOp = Nz | Neg | Abs | Round | Nearest | Ceil | Floor | Sqrt | Log10 | Log


type BinOp = 
    | Add | Sub | Mul | Div | Sl | Sr | Rl | Rr | CopySign | Max | Min
    | And | Or | Xor | Imply


type RelOp = Eq | Nq | Le | Lt | Ge | Gt


type TypeDec<'id, 'info> = Ty of 'id * Type<'id>


type TypeInstance<'id> =
    | Primitive of Operand<int8, int16, int32, int64, uint8, uint16, uint32, uint64, float32, float>
    | Array of TypeInstance<'id> list
    | Struct of ('id * TypeInstance<'id>) list    


type Expr<'id, 'info> = 
    | Value of TypeInstance<'id> * 'info
    | Var of 'id * 'info 
    | New of Type<'id> * Expr<'id,'info> list
    | IndexOf of 'id * Expr<'id, 'info> * 'info
    | FieldOf of 'id * 'id * 'info
    | Unary of UnOp    * Expr<'id, 'info> * 'info
    | Binary of BinOp  * Expr<'id, 'info> * Expr<'id, 'info> * 'info
    | Compare of RelOp * Expr<'id, 'info> * Expr<'id, 'info> * 'info
    | Cvt of ty * Expr<'id, 'info> * 'info


type Stmt<'id, 'info> =
    | Assign of 'id * Expr<'id, 'info> * 'info
    | AssignIndex of 'id * Expr<'id, 'info> * Expr<'id, 'info> * 'info
    | AssignField of 'id * 'id * Expr<'id, 'info> * 'info
    | If of Expr<'id, 'info> * Expr<'id, 'info> list * Expr<'id, 'info> list * 'info
    | When of Expr<'id, 'info> * Expr<'id, 'info> list * 'info // if without else
    | While of Expr<'id, 'info> * Stmt<'id, 'info> list * 'info
    | Return of Expr<'id, 'info> * 'info


type Function<'id, 'info> =
    {
        Name: 'id
        Vars: 'id list
        Locals: 'id list
        Body: Stmt<'id, 'info> list
        Info: 'info
    }


type Data<'id,'info> =
    {
        Name: 'id
        Type: Type<'id>
        Content: TypeInstance<'id>
        Info: 'info
    }


type Memory<'id,'info> =
    {
        size: int
        adr: nativeint
    }


type Module<'id,'info> =
    {
        Name: string
        Location: string list
        Types: Type<'id> list
        Memory: Memory<'id,'info> list
        Functions: Function<'id,'info> list
        Data: TypeInstance<'id> list
        Start: 'id option
    }

