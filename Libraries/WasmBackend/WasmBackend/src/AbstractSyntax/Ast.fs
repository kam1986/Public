
module Wasm.AST
open Types

type Value =
    | I of size: byte * value: int64
    | F of size: byte * Value: float


type Sign = Signed | UnSigned with
    override Sign.ToString() = 
        match Sign with 
        | Signed -> "_s" 
        | _ -> "_u"


type iunop = CLZ | CTZ | POPCNT with 
    override i.ToString() =
        match i with
        | CLZ       -> ".clz"
        | CTZ       -> ".ctz"
        | POPCNT    -> ".popcnt"

type ibinop = 
    | ADD | SUB | MUL | DIV | REM
    | AND | OR  | XOR | SHL | SHR of Sign | ROTL | ROTR
with
    override b.ToString() =
        match b with
        | ADD   -> ".add"
        | SUB   -> ".sub"
        | MUL   -> ".mul"
        | DIV   -> ".div"
        | REM   -> ".rem"
        | AND   -> ".and"
        | OR    -> ".or"
        | XOR   -> ".xor"
        | SHL   -> ".shl"
        | SHR s -> ".shr" + string s // overload for the sign will result in inn.shr_u or inn.shr_s 
        | ROTL  -> ".rotl"
        | ROTR  -> ".rotr"

type funop = ABS | NEG | SQRT | CEIL | FLOOR | TRUNC | NEAREST with
    override f.ToString() =
        match f with
        | ABS       -> ".abs"
        | NEG       -> ".neg"
        | SQRT      -> ".sqrt"
        | CEIL      -> ".ceil"
        | FLOOR     -> ".floor"
        | TRUNC     -> ".trunc"
        | NEAREST   -> ".nearest"

type fbinop = Add | Sub | Mul | Div | Min | Max | CopySign with
    override f.ToString() =
        match f with
        | Add       -> ".add"
        | Sub       -> ".sub"
        | Mul       -> ".mul"
        | Div       -> ".div"
        | Min       -> ".min"
        | Max       -> ".max"
        | CopySign  -> ".copysign"

type itestop = EQZ with override i.ToString() = ".eqz"

type irelop = EQ | NE | LT of Sign | GT of Sign | LE of Sign | GE of Sign with
    override i.ToString() =
        match i with
        | EQ    -> ".eq"
        | NE    -> ".ne"
        | LT s  -> ".lt" + string s 
        | GT s  -> ".gt" + string s
        | LE s  -> ".le" + string s
        | GE s  -> ".ge" + string s

type frelop = Eq | Ne | Lt | Gt | Le | Ge with
    override op.ToString() =
        match op with
        | Eq  -> ".eq"
        | Ne  -> ".ne"
        | Lt  -> ".lt"  
        | Gt  -> ".gt" 
        | Le  -> ".le"
        | Ge  -> ".ge" 



type unop = Funop of funop | Iunop of iunop with
    override op.ToString() =
        match op with
        | Funop op -> op.ToString()
        | Iunop op -> op.ToString()


type binop = Fbinop of fbinop | Ibinop of ibinop
type testop = itestop
type relop = Irelop of irelop | Frelop of frelop



// we use nativeint for interpreter reasons
// the reduction on cases is for ease of implementation.
// type checking can be done easily by matching the limited 
// cases that is true to true or else false
type Instr =
    // numeric instructions
    | Const         of NumType * Value
    | UOP           of NumType * unop
    | Bop           of NumType * binop
    | TOP           of NumType * itestop
    | ROP           of NumType * relop
    | Extend_s      of NumType * NumType 
    | I32Wrap64 
    | Extend        of Sign // assume size i32 -> i64
    | Trunc         of Sign * NumType * NumType
    | Trunc_Sat     of Sign * NumType * NumType
    | Demote 
    | Promote
    | Convert       of Sign * NumType * NumType 
    | ReInterpret   of NumType * NumType


    // reference instructions
    | RefNull       of RefType
    | RefIsNul
    | RefFunc       of funcidx


    // parametric instructions
    | Drop
    | Select        of ValType option // in specification it is an optional valtype where numeric is the None case


    // variable instructions
    | LocalGet      of localidx
    | LocalSet      of localidx
    | LocalTee      of localidx // like local.set but leave the value on the top of the stack
    | GlobalGet     of globalidx
    | GLobalSet     of globalidx
    

    // Table instructions
    | TableGet      of tableidx
    | TableSet      of tableidx
    | TableSize     of tableidx
    | TableGrow     of tableidx
    | TableFill     of tableidx
    | TableCopy     of tableidx * tableidx
    | TableInit     of tableidx * elemidx
    | ElemDrop      of elemidx

    
    // Memory Instructions
    | Load          of (Sign * NumType) * NumType * memarg
    | Store         of (Sign * NumType) * NumType * memarg
    | MemSize
    | MemGrow
    | MemFill
    | MemCopy
    | MemInint      of dataidx
    | DataDrop      of dataidx


    // Control Instructions
    | Nop
    | UnReachable
    | Block         of BlockType * Instr vec
    | Loop          of BlockType * Instr vec
    | If            of BlockType * Instr vec * Instr vec
    | Br            of labelidx 
    | Br_if         of labelidx
    | Br_table      of labelidx vec * labelidx
    | Return
    | Call          of funcidx
    | Call_Indirect of tableidx * typeidx
with
    member I.ToWat() = I.ToWat 0
    // Important to notice that we do not check if the argument are valid to the instruction
    // This is the type checkers job 
    member internal I.ToWat (indent : int) = 
        let indent' = [|for i in 1 .. indent -> "    "|] |> Array.fold (+) ""
        match I with
        | Const (numtype, value) -> 
            string numtype + ".const " + string value
        
        | UOP (numtype, op)  ->
            string numtype + string op
        
        | TOP (numtype, op) -> string numtype + string op
        | ROP (numtype, op) -> string numtype + string op
        | Extend_s (numtype1, numtype2) -> 
            string numtype1 + ".extend" + (string numtype2).[1..] + "_s"
        
        | I32Wrap64     -> "i32.wrap_i64"
        | Extend    s   -> "i64.extendi32" + string s 
        
        | Trunc     (s, numtype1, numtype2) ->
            string numtype1 + ".trunc_" + string numtype2 + string s
        
        | Trunc_Sat (s, numtype1, numtype2) -> 
            string numtype1 + ".trunc_sat_" + string numtype2 + string s
        
        | Demote    -> "f32.demote_f64" 
        
        | Promote   -> "f64.promote_f32"
        
        | Convert     (s, numtype1, numtype2) -> 
            string numtype1 + ".convert_" + string numtype2 + string s
        
        | ReInterpret (numtype1, numtype2) ->
            string numtype1 + ".reinterpret_" + string numtype2
        

        | RefNull reftype -> "ref.null " + string reftype  
        | RefIsNul -> "ref.is_null"
        | RefFunc funcidx -> "ref.func " + string funcidx


        | Drop  -> "drop"
        | Select valtype -> 
            match valtype with
            | Some numtypes -> ""
            | _ -> string valtype
            |> (+) "select "

        | LocalGet  localidx  -> "local.get " + string localidx
        | LocalSet  localidx  -> "local.set " + string localidx
        | LocalTee  localidx  -> "local.tee " + string localidx
        | GlobalGet globalidx -> "global.get " + string globalidx
        | GLobalSet globalidx -> "global.set " + string globalidx
           

        // Table instructions
        | TableGet  tableidx    -> "table.get " + string tableidx 
        | TableSet  tableidx    -> "table.set "  + string tableidx
        | TableSize tableidx    -> "table.size "  + string tableidx
        | TableGrow tableidx    -> "table.grow "  + string tableidx
        | TableFill tableidx    -> "table.copy "  + string tableidx
        | TableCopy (tableidx1, tableidx2) -> 
            "table.copy " + string tableidx1 + " " + string tableidx2 
        
        | TableInit (tableidx, elemidx) -> 
            "table.init " + string tableidx + " " + string elemidx
            
        | ElemDrop  elemidx -> "table.drop " + string elemidx

        // Memory Instructions
        | s -> sprintf "%A: is not yet implemented" s
        |> (+) indent' // prefix with the proper indentation to the current scope

type Expr = Instr seq // end mark

// a function declaration
type func       = Func of type': typeidx * locals: ValType vec * body: Expr

type table      = TableType // alloc memory with only one type in it
type mem        = MemType   // allocated raw data
type global'    = GlobalType * Expr // the expr are either immutable or mutable and are given by an init expr

type elemmode   = Passive | Declarative | Active of tableidx * offset: Expr
type elem       = Elem of typ: RefType * init: Expr vec * mode: elemmode

type datamode   = Passive | Active of memory: memidx * offset: Expr
type data       = Data of init: byte vec * mode: datamode
type start      = funcidx

type exportdesc =
    | Func   of funcidx
    | Table  of tableidx
    | Mem    of memidx
    | Global of globalidx

type export = Export of name: string * desc: exportdesc

type importdesc = 
    | Func of typeidx
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

type import     = Import of modulename : string * name: string * importdesc

(*
type Module =
    | Type      of FuncType vec
    | Funcs     of func vec
    | Tables    of table vec
    | Mems      of mem vec
    | Globals   of global' vec
    | Elems     of elem vec
    | Datas     of data vec
    | Start     of start option
    | Imports   of import vec
    | Exports   of export vec
*)


type Module =
    {
            Type: FuncType seq
            Funcs: func seq
            Tables: table seq
            Mems: mem seq
            Globals: global' seq
            Elems: elem seq
            Data: data seq
            Start: start option
            Imports: import seq
            Exports: export seq
    }

    static member empty =
        {
            Type = seq[]
            Funcs = seq[]
            Tables = seq[]
            Mems = seq[]
            Globals = seq[]
            Elems = seq[]
            Data = seq[]
            Start = None
            Imports = seq[]
            Exports = seq[]
        }