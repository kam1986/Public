module wasm

open Helpers
open Wasm
open Syntax
open Types
open Values
(*
type OP<'i,'f> = Syntax.op<'i,'f>

exception WasifyError of Message: string

let WError msg =
    WasifyError $"Wasify Error {msg}"
    |> raise


let (~&) item = seq[item]

let (+>) item sq = seq[yield item; yield! sq]

let (<+) sq item = seq[yield! sq; yield item]

let (<+>) sq1 sq2 = seq[yield! sq1; yield! sq2]

let (++) item1 item2 = seq[yield item1; yield item2]


// at some point this will handle fetching the correct error message and printing it before crashing
let errorRecovery point = seq[]

let rec GetType e =
    match e with
    | Convert(OP.I32 _,_)
    | Value (OP.I32 _,_) -> OP.I32()
    | Convert(OP.F64 _,_)
    | Value (OP.F64 _,_) -> OP.F64()
    | Binary(_,l,r,_) ->
        let lt = GetType l
        let rt = GetType r
        if rt = lt then
            rt
        else
            failwith "not well typed"

    | Unary(_, e, _) -> GetType e

let WasifyValue v info =
    match v with
    | OP.I32 v -> Const(num.I32 v, info)
    | OP.U32 v -> Const(num.U32 (uint v), info)
    | OP.F32 v -> Const(num.F32 (float32 v), info)
    | OP.F64 v -> Const(num.F64 v, info)

let WasifyBinop op t info =
    match t with
    | OP.I32 _ ->
        match op with 
        | Add -> instr.Binary(I32 IntOp.Add, info)
        | Sub -> instr.Binary(I32 IntOp.Sub, info)
        | Mul -> instr.Binary(I32 IntOp.Mul, info)
        | Div -> instr.Binary(I32 IntOp.Div, info)
    
    | OP.F64 _ ->
        match op with 
        | Add -> instr.Binary(F64 FloatOp.Add, info)
        | Sub -> instr.Binary(F64 FloatOp.Sub, info)
        | Mul -> instr.Binary(F64 FloatOp.Mul, info)
        | Div -> instr.Binary(F64 FloatOp.Div, info)

let StoreVariable v = seq[]
let LoadVariable v = seq[]


let rec WasifyExpr e =
    match e with
    | Value(v, info) -> &WasifyValue v info
    | Binary(op, left, right, info) ->
        let op = WasifyBinop op (GetType left) info

        WasifyExpr left <+> WasifyExpr right <+ op

    | Convert(e,info) ->
        match e with
        | OP.I32 e -> WasifyExpr e <+ instr.Convert(I32 IntOp.cvtop.TruncF64, info)
        | OP.F64 e -> WasifyExpr e <+ instr.Convert(F64 FloatOp.cvtop.ConvertSI32, info)

    | Unary(op, e, info) ->
        let code = WasifyExpr e

        match GetType e with
        | OP.I32 _ ->
            match op with
            | Neg  -> Const(I32 0, info) +> code <+ instr.Binary(I32 IntOp.Sub, info)
            | Not  -> code <+ instr.Test(I32 IntOp.testop.Eqz, info)
            | Pc   -> code <+ instr.Unary(I32 IntOp.Popcnt, info)                    
            | Clz  -> code <+ instr.Unary(I32 IntOp.Clz, info)
            | Ctz  -> code <+ instr.Unary(I32 IntOp.Ctz, info)
            | Sqrt -> 
                // the effect are simular to int(sqrt(float32(e)))
                code <+ 
                instr.Convert(F32 FloatOp.ConvertSI32, info) <+  
                instr.Unary(F32 FloatOp.Sqrt, info) <+
                instr.Convert(I32 IntOp.cvtop.TruncF32, info) 
            | Round   -> code 
            | Nearest -> code
            | Floor   -> code
            | Ceil    -> code
            | Abs     -> WError $"should have been expanded before this point"


*)