namespace Wasm

//#nowarn "25" "64" "667"

module Encode =
    open System.Text

    open Helpers

    open AbstractSyntax.Wasm
    open Wasm
    open Types
    open Values


    let (=>) sq1 sq2 = 
        seq[
            yield! sq1
            yield! sq2
        ]

    let inline (~&) item = seq[byte item]


    let inline u64 value = 
        let value = uint64 value
        let rec loop bytes b n =
            match n with
            | 0UL -> List.rev ((b &&& 127uy) :: bytes)
            | _ -> loop ((b ||| 128uy) :: bytes) (byte n) (n >>> 7)

        loop [] (byte value) (value >>> 7)
        
    let inline u32 value = u64(uint64 value)

    let inline i64 value =
        let value = int64 value
        let rec loop bytes b n =
            match n with
            |  0L when b &&& 0x40uy = 0uy  -> List.rev ((b &&& 127uy) :: bytes) 
            | -1L when b &&& 0x40uy <> 0uy -> List.rev ((b &&& 127uy) :: bytes)
            | _ -> loop ((b ||| 128uy) :: bytes) (byte n) (n >>> 7)

        loop [] (byte value) (value >>> 7)
    

    let inline i32 value = i64 value

    let inline f64 (value: float) = System.BitConverter.GetBytes value 

    let inline f32 (value: float32) = System.BitConverter.GetBytes value

    let GenName (name: string) = 
        let bytes = Encoding.UTF8.GetBytes name
        u32(uint bytes.Length) => bytes


    let GenVec f (sq: _ seq) = u32(Seq.length sq) => Seq.concat (Seq.map f sq) 

    let GenNumType = function
        | I32_t -> &0x7F
        | I64_t -> &0x7E
        | F32_t -> &0x7D
        | F64_t -> &0x7C

    let GenRefType = function
        | refType.FuncRef   -> &0x70
        | refType.ExternRef -> &0x6F
    

    let GenValueType = function
        | NumType nt -> GenNumType nt
        | RefType ft -> GenRefType ft

    let GenResulType (Result rts) = GenVec GenValueType rts

    let GenFuncType (FuncType(input,output)) =  &0x60uy => GenResulType input => GenResulType output

    let GenLimits = function
        | { min = m;  max = None    } -> &0x00uy => u32(uint m)
        | { min = m1; max = Some m2 } -> &0x01uy => u32(uint m1) => u32(uint m2)


    let GenMemType (MemoryType limits) = GenLimits limits

    let GenTableType (TableType (limits, rt)) = GenRefType rt => GenLimits limits

    let GenMut = function
        | Immutable -> &0x00
        | Mutable   -> &0x01

    let GenGlobalType (GlobalType(vt, mut)) = GenValueType vt => GenMut mut


    let GenBlockType = function
        | ValBlockType None     -> seq[]
        | ValBlockType (Some t) -> GenValueType t
        | VarBlockType x        -> i64 x :> _ seq

    let End = &0x0B 
    let Else = &0x05
    let Nothing = seq[] : byte seq
     
    let inline Idx i = u32 i |> List.toSeq

    let GenMemArg (memarg : memop<_,_>) = u32 memarg.align => u32 memarg.offset

         



    let rec GenInstruction = function
        //| Unreachable _     -> &0x00
        | Nop _             -> &0x01
        | Block(bt, body,_) -> 
            &0x02
            => GenBlockType bt 
            => Seq.fold (fun code instr -> code => GenInstruction instr) Nothing body 
            => End

        | Loop(bt, body,_) -> 
            &0x03
            => GenBlockType bt 
            => Seq.fold (fun code instr -> code => GenInstruction instr) Nothing body 
            => End

        | If(bt, truebranch, falsebranch, _) ->
            &0x04 
            => GenBlockType bt
            => Seq.fold (fun code instr -> code => GenInstruction instr) Nothing truebranch
            => if Seq.isEmpty falsebranch then Nothing else Else  
            => Seq.fold (fun code instr -> code => GenInstruction instr) Nothing falsebranch
            => End

        | Br(idx, _)                    -> &0x0C => Idx idx
        | BrIf(idx, _)                  -> &0x0D => Idx idx
        | BrTable(idxs, idx, _)         -> &0x0E => GenVec Idx idxs => Idx idx
        | Return _                      -> &0x0F
        | Call(idx, _)                  -> &0x10 => Idx idx
        | CallIndirect(idx1, idx2, _)   -> &0x11 => Idx idx1 => Idx idx2

        | RefNull(rt, _)                -> &0xD0 => GenRefType rt
        | RefIsNull _                   -> &0xD1
        | RefFunc(x, _)                 -> &0xD2 => Idx x
     
        | Drop(Stack, _ )               -> &0x1A
        | Select([], _)                 -> &0x1B
        | Select(tys, _)                -> &0x1C => GenVec GenValueType tys
        | Load(Local x, _)              -> &0x20 => Idx x
        | Store(Local x, _)             -> &0x21 => Idx x
        | Tee(Local x, _)               -> &0x22 => Idx x
        | Load(Global x, _)             -> &0x23 => Idx x
        | Store(Global x, _)            -> &0x24 => Idx x
        | Tee(Global x, _)              -> &0x24 => Idx x => &0x23 => Idx x
        | Load(Table x, _)              -> &0x25 => Idx x
        | Store(Table x, _)             -> &0x26 => Idx x 
        | Tee(Table x, _)               -> &0x26 => Idx x => &0x25 => Idx x
        | Init(Table x, Table y, _)     -> &0xFC => u32 12 => Idx x => Idx y 
        | Drop(Table x, _)              -> &0xFC => u32 13 => Idx x 
        | Copy(Table x, Table y, _)     -> &0xFC => u32 12 => Idx x => Idx y 
        | Grow(Table x, _)              -> &0xFC => u32 15 => Idx x 
        | Size(Table x, _)              -> &0xFC => u32 16 => Idx x
        | Fill(Table x, _)              -> &0xFC => u32 17 => Idx x

        | Load(Memory ({ ty = I32_t; sz = Some(Pack8, SX)} as mem), _)  -> &0x2C => GenMemArg mem    
        | Load(Memory ({ ty = I32_t; sz = Some(Pack8, ZX)} as mem), _)  -> &0x2D => GenMemArg mem
        | Load(Memory ({ ty = I32_t; sz = Some(Pack16, SX)} as mem), _) -> &0x2E => GenMemArg mem
        | Load(Memory ({ ty = I32_t; sz = Some(Pack16, ZX)} as mem), _) -> &0x2F => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack8, SX)} as mem), _)  -> &0x30 => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack8, ZX)} as mem), _)  -> &0x31 => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack16, SX)} as mem), _) -> &0x32 => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack16, ZX)} as mem), _) -> &0x33 => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack32, SX)} as mem), _) -> &0x34 => GenMemArg mem
        | Load(Memory ({ ty = I64_t; sz = Some(Pack32, ZX)} as mem), _) -> &0x35 => GenMemArg mem
        // when non of the above cases match
        | Load(Memory mem, _) when mem.ty = I32_t -> &0x28 => GenMemArg mem
        | Load(Memory mem, _) when mem.ty = I64_t -> &0x29 => GenMemArg mem
        | Load(Memory mem, _) when mem.ty = F32_t -> &0x2A => GenMemArg mem
        | Load(Memory mem, _) when mem.ty = F64_t -> &0x2B => GenMemArg mem 
    

        | Store(Memory ( {ty = I32_t; sz = Some (Pack8, _) } as mem), _)   -> &0x3A => GenMemArg mem
        | Store(Memory ( {ty = I32_t; sz = Some (Pack16,_) } as mem), _)   -> &0x3B => GenMemArg mem
    
        | Store(Memory ( {ty = I64_t; sz = Some (Pack8,  _) } as mem), _)  -> &0x3C => GenMemArg mem
        | Store(Memory ( {ty = I64_t; sz = Some (Pack16, _) } as mem), _)  -> &0x3D => GenMemArg mem
        | Store(Memory ( {ty = I64_t; sz = Some (Pack32, _) } as mem), _)  -> &0x3E => GenMemArg mem
        
        | Store(Memory (mem: memop<_,_>), _) when mem.ty = I32_t -> &0x36 => GenMemArg mem
        | Store(Memory (mem: memop<_,_>), _) when mem.ty = I64_t -> &0x37 => GenMemArg mem
        | Store(Memory (mem: memop<_,_>), _) when mem.ty = F32_t -> &0x38 => GenMemArg mem
        | Store(Memory (mem: memop<_,_>), _) when mem.ty = F64_t -> &0x39 => GenMemArg mem 

        | Size(Memory _, _)                     -> &0x3F => &0x00
        | Grow(Memory _, _)                     -> &0x40 => &0x00
        | Init(_, Memory mem, _)                -> &0xFC => u32 8 => Idx mem.offset => &0x00
        | Drop(Memory mem, _)                   -> &0xFC => u32 9 => Idx mem.offset
        | Copy(Memory _, Memory _, _)           -> &0xFC => u32 10 => &0x00 => &0x00
        | Fill(Memory _, _)                     -> &0xFC => u32 11 => &0x00 
        | Const(I32 v, _)                          -> &0x41 => i32 v
        | Const(U32 v, _)                          -> &0x41 => i32 v
        | Const(I64 v, _)                          -> &0x42 => i64 v
        | Const(U64 v, _)                          -> &0x42 => i64 v
        | Const(F32 v, _)                          -> &0x43 => f32 v
        | Const(F64 v, _)                          -> &0x44 => f64 v
        | Unary(I32 op, _) | Unary(U32 op, _)   -> 
            match op with
            | IntOp.unop.Clz       -> &0x67 
            | IntOp.unop.Ctz       -> &0x68
            | IntOp.unop.Popcnt    -> &0x69

        | Compare(I32 op, _) ->
            match op with
            | IntOp.relop.Eq -> &0x46
            | IntOp.relop.Ne -> &0x47
            | IntOp.relop.Lt -> &0x48
            | IntOp.relop.Gt -> &0x4A
            | IntOp.relop.Le -> &0x4C
            | IntOp.relop.Ge -> &0x4E
    
        | Compare(U32 op, _) ->
            match op with
            | IntOp.relop.Eq -> &0x46
            | IntOp.relop.Ne -> &0x47
            | IntOp.relop.Lt -> &0x49
            | IntOp.relop.Gt -> &0x4B
            | IntOp.relop.Le -> &0x4D
            | IntOp.relop.Ge -> &0x4F

        | Binary(I32 op, _) ->
            match op with
            | IntOp.binop.Add  -> &0x6A
            | IntOp.binop.Sub  -> &0x6B
            | IntOp.binop.Mul  -> &0x6C
            | IntOp.binop.Div  -> &0x6D
            | IntOp.binop.Rem  -> &0x6F
            | IntOp.binop.And  -> &0x71
            | IntOp.binop.Or   -> &0x72
            | IntOp.binop.Xor  -> &0x73
            | IntOp.binop.Shl  -> &0x74
            | IntOp.binop.Shr  -> &0x75
            | IntOp.binop.Rotl -> &0x77
            | IntOp.binop.Rotr -> &0x78

        | Binary(U32 op, _) ->
            match op with
            | IntOp.binop.Add  -> &0x6A
            | IntOp.binop.Sub  -> &0x6B
            | IntOp.binop.Mul  -> &0x6C
            | IntOp.binop.Div  -> &0x6E
            | IntOp.binop.Rem  -> &0x70
            | IntOp.binop.And  -> &0x71
            | IntOp.binop.Or   -> &0x72
            | IntOp.binop.Xor  -> &0x73
            | IntOp.binop.Shl  -> &0x74
            | IntOp.binop.Shr  -> &0x76
            | IntOp.binop.Rotl -> &0x77
            | IntOp.binop.Rotr -> &0x78


        | Convert(I32 op, _) ->
            match op with 
            | IntOp.cvtop.WrapI64   -> &0xA7
            | IntOp.cvtop.TruncF32  -> &0xA8
            | IntOp.cvtop.TruncF64  -> &0xAA
            | _ -> Failure "convertion operand for i32 not defined" |> raise
        
        | Convert(U32 op, _) ->
            match op with 
            | IntOp.cvtop.WrapI64   -> &0xA7
            | IntOp.cvtop.TruncF32  -> &0xA9
            | IntOp.cvtop.TruncF64  -> &0xAB
            | _ -> Failure "convertion operand for i32 not defined" |> raise
        
        | Convert(I64 op, _) ->
            match op with 
            | IntOp.cvtop.Extend    -> &0xAC
            | IntOp.cvtop.TruncF32  -> &0xAE
            | IntOp.cvtop.TruncF64  -> &0xB0
            | _ -> Failure "convertion operand for i64 not defined" |> raise
    
        | Convert(U64 op, _) ->
            match op with 
            | IntOp.cvtop.Extend    -> &0xAD
            | IntOp.cvtop.TruncF32  -> &0xAF
            | IntOp.cvtop.TruncF64  -> &0xB1
            | _ -> Failure "convertion operand for i64 not defined" |> raise   
       
        | Unary(I64 op, _) | Unary(U64 op, _)   -> 
            match op with
            | IntOp.unop.Clz       -> &0x79 
            | IntOp.unop.Ctz       -> &0x7A
            | IntOp.unop.Popcnt    -> &0x7B

        | Compare(I64 op, _) ->
            match op with
            | IntOp.relop.Eq -> &0x51
            | IntOp.relop.Ne -> &0x52
            | IntOp.relop.Lt -> &0x53
            | IntOp.relop.Gt -> &0x55
            | IntOp.relop.Le -> &0x57
            | IntOp.relop.Ge -> &0x59
   
        | Compare(U64 op, _) ->
            match op with
            | IntOp.relop.Eq -> &0x51
            | IntOp.relop.Ne -> &0x52
            | IntOp.relop.Lt -> &0x54
            | IntOp.relop.Gt -> &0x56
            | IntOp.relop.Le -> &0x58
            | IntOp.relop.Ge -> &0x5A

        | Binary(I64 op, _) ->
            match op with
            | IntOp.binop.Add  -> &0x7C
            | IntOp.binop.Sub  -> &0x7D
            | IntOp.binop.Mul  -> &0x7E
            | IntOp.binop.Div  -> &0x7F
            | IntOp.binop.Rem  -> &0x81
            | IntOp.binop.And  -> &0x83
            | IntOp.binop.Or   -> &0x84
            | IntOp.binop.Xor  -> &0x85
            | IntOp.binop.Shl  -> &0x86
            | IntOp.binop.Shr  -> &0x87
            | IntOp.binop.Rotl -> &0x89
            | IntOp.binop.Rotr -> &0x8A

        | Binary(U64 op, _) ->
            match op with
            | IntOp.binop.Add  -> &0x7C
            | IntOp.binop.Sub  -> &0x7D
            | IntOp.binop.Mul  -> &0x7E
            | IntOp.binop.Div  -> &0x80
            | IntOp.binop.Rem  -> &0x82
            | IntOp.binop.And  -> &0x83
            | IntOp.binop.Or   -> &0x84
            | IntOp.binop.Xor  -> &0x85
            | IntOp.binop.Shl  -> &0x86
            | IntOp.binop.Shr  -> &0x88
            | IntOp.binop.Rotl -> &0x89
            | IntOp.binop.Rotr -> &0x8A


        | Spec(I32 op, _) ->
            match op with
            | IntOp.spec.ExtendS p    ->
                match p with
                | Pack8  -> &0xC0
                | Pack16 -> &0xC1
                | _ -> Failure "convertion operand for i32 not defined" |> raise
        
            | IntOp.spec.TruncSatF32 -> &0xFC => u32 0u
            | IntOp.spec.TruncSatF64 -> &0xFC => u32 2u
            | IntOp.spec.Reinterpret -> &0xBC

        | Spec(U32 op, _) ->
            match op with
            | IntOp.spec.ExtendS p    ->
                match p with
                | Pack8  -> &0xC0
                | Pack16 -> &0xC1
                | _ -> Failure "convertion operand for i32 not defined" |> raise
                
            | IntOp.spec.TruncSatF32 -> &0xFC => u32 1u
            | IntOp.spec.TruncSatF64 -> &0xFC => u32 3u
            | IntOp.spec.Reinterpret -> &0xBC


        | Spec(I64 op, _) ->
            match op with
            | IntOp.spec.ExtendS p   ->
                match p with
                | Pack8  -> &0xC2
                | Pack16 -> &0xC3
                | Pack32 -> &0xC4

            | IntOp.spec.TruncSatF32 -> &0xFC => u32 4u
            | IntOp.spec.TruncSatF64 -> &0xFC => u32 6u
            | IntOp.spec.Reinterpret -> &0xBD


        | Spec(U64 op, _) ->
            match op with
            | IntOp.spec.ExtendS p   ->
                match p with
                | Pack8  -> &0xC2
                | Pack16 -> &0xC3
                | Pack32 -> &0xC4
            | IntOp.spec.TruncSatF32 -> &0xFC => u32 5u
            | IntOp.spec.TruncSatF64 -> &0xFC => u32 7u
            | IntOp.spec.Reinterpret -> &0xBD

        | Unary(F32 op, _) ->
            match op with
            | FloatOp.unop.Abs      -> &0x8B
            | FloatOp.unop.Neg      -> &0x8C
            | FloatOp.unop.Ceil     -> &0x8D
            | FloatOp.unop.Floor    -> &0x8E
            | FloatOp.unop.Trunc    -> &0x8F
            | FloatOp.unop.Nearest  -> &0x90
            | FloatOp.unop.Sqrt     -> &0x91
    
        | Compare(F32 op, _) ->
            match op with
            | FloatOp.relop.Eq -> &0x5B
            | FloatOp.relop.Ne -> &0x5C
            | FloatOp.relop.Lt -> &0x5D
            | FloatOp.relop.Gt -> &0x5E
            | FloatOp.relop.Le -> &0x5F
            | FloatOp.relop.Ge -> &0x60

        | Unary(F64 op, _) ->
            match op with
            | FloatOp.unop.Abs      -> &0x99
            | FloatOp.unop.Neg      -> &0x9A
            | FloatOp.unop.Ceil     -> &0x9B
            | FloatOp.unop.Floor    -> &0x9C
            | FloatOp.unop.Trunc    -> &0x9D
            | FloatOp.unop.Nearest  -> &0x9E
            | FloatOp.unop.Sqrt     -> &0x9F


        | Compare(F64 op, _) ->
            match op with
            | FloatOp.relop.Eq -> &0x61
            | FloatOp.relop.Ne -> &0x62
            | FloatOp.relop.Lt -> &0x63
            | FloatOp.relop.Gt -> &0x64
            | FloatOp.relop.Le -> &0x65
            | FloatOp.relop.Ge -> &0x66

        | Binary(F32 op, _) ->
            match op with
            | FloatOp.binop.Add         -> &0x92
            | FloatOp.binop.Sub         -> &0x93
            | FloatOp.binop.Mul         -> &0x94
            | FloatOp.binop.Div         -> &0x95
            | FloatOp.binop.Min         -> &0x96
            | FloatOp.binop.Max         -> &0x97
            | FloatOp.binop.CopySign    -> &0x98

        | Binary(F64 op, _) ->
            match op with
            | FloatOp.binop.Add         -> &0xA0
            | FloatOp.binop.Sub         -> &0xA1
            | FloatOp.binop.Mul         -> &0xA2
            | FloatOp.binop.Div         -> &0xA3
            | FloatOp.binop.Min         -> &0xA4
            | FloatOp.binop.Max         -> &0xA5
            | FloatOp.binop.CopySign    -> &0xA6

        | Convert(F32 op, _) ->
            match op with
            | FloatOp.cvtop.ConvertSI32 -> &0XB2
            | FloatOp.cvtop.ConvertUI32 -> &0XB3
            | FloatOp.cvtop.ConvertSI64 -> &0XB4
            | FloatOp.cvtop.ConvertUI64 -> &0XB5
            | FloatOp.cvtop.DemoteF64   -> &0XB6
            | _ -> Failure "convertion operand for f32 not defined" |> raise 

         | Convert(F64 op, _) ->
             match op with
             | FloatOp.cvtop.ConvertSI32 -> &0XB7
             | FloatOp.cvtop.ConvertUI32 -> &0XB8
             | FloatOp.cvtop.ConvertSI64 -> &0XB9
             | FloatOp.cvtop.ConvertUI64 -> &0XBA
             | FloatOp.cvtop.PromoteF32  -> &0XBB
             | _ -> Failure "convertion operand for f32 not defined" |> raise 


         | Spec(F32 _, _) -> &0xBE
         | Spec(F64 _, _) -> &0xBF
         | Test(I32 _, _) | Test(U32 _, _) -> &0x45
         | Test(I64 _, _) | Test(U64 _, _) -> &0x50
         
         | instruction -> Failure $"Instruction incoding not covered {instruction}" |> raise
      
    let GenExpr (expr.Expr instructions) =
        Seq.fold (fun instrs instr -> instrs => GenInstruction instr) (seq[]) instructions => End


    let Section id f content =
        let content = f content
        if content = seq[0uy] then
            seq[]
        else
            &id => Idx (Seq.length content) => content

    let CostumeSection name B = Section 0x00 id (GenName name => B)


    let TypeSection fts = Section 0x01 (GenVec GenFuncType) fts


    let GenImportDesc = function
        | ImFunc x    -> &0x00 => Idx x
        | ImTable tt  -> &0x01 => GenTableType tt
        | ImMemory mt -> &0x02 => GenMemType mt
        | ImGlobal gt -> &0x03 => GenGlobalType gt

    let GenImport { modulename = mn; name = n; desc = d } = GenName mn => GenName n => GenImportDesc d

    let ImportSection imports = Section 0x02 (GenVec GenImport) imports

    let FunctionSection fs = Section 0x03 (GenVec (fun (f: func<_,_,_,_>) -> Idx f.ty)) fs

    let TableSection ts = Section 0x04 (GenVec (fun (t : table) -> GenTableType t.ty)) ts

    let MemorySection ms = Section 0x05 (GenVec (fun (t : memory) -> GenMemType t.ty)) ms

    let GenGlobal (g : Global<_,_,_,_>) = GenGlobalType g.ty => GenExpr g.init

    let GlobalSection gs = Section 0x06 (GenVec GenGlobal) gs

    let GenExportDesc = function
        | ExFunc id   -> &0x00 => Idx id
        | ExTable id  -> &0x01 => Idx id
        | ExMemory id -> &0x02 => Idx id 
        | ExGlobal id -> &0x03 => Idx id

    let GenExport ({ name = name; desc = d }: export)  = GenName name => GenExportDesc d

    let ExportSection es = Section 0x07 (GenVec GenExport) es

    let StartSection { func = start } = Section 0x08 (function None -> seq[0uy] | Some id -> Idx id) start


    let OnlyIdx exprs = 
        List.filter (fun (Expr e) -> match e with [RefFunc _] -> true  | _ -> false) exprs
        |> List.isEmpty 
        |> not

    let rec GetFuncIdx exprs = 
        match exprs with
        | []                            -> []
        | (Expr[RefFunc(y,_)]) :: exprs -> y :: GetFuncIdx exprs
    
    let elmkind = &0x00

    let GenElement = function
        | { ty = refType.FuncRef; init = y ; mode = elemmode.Active frame } when frame.index = 0 && OnlyIdx y -> 
            &0x00 => GenExpr frame.offset => GenVec Idx (GetFuncIdx y)

        | { ty = refType.FuncRef; init = el ; mode = elemmode.Active frame } when frame.index = 0 -> 
            &0x04 => GenExpr frame.offset => GenVec GenExpr el
    
        | { ty = ty; init = y; mode = elemmode.Passive} when OnlyIdx y -> 
            &0x01 => GenRefType ty => elmkind => GenVec Idx (GetFuncIdx y)

        | { ty = t; init = y ; mode = elemmode.Active frame } when OnlyIdx y ->
            &0x02 => Idx frame.index => GenExpr frame.offset => GenRefType t => elmkind => GenVec Idx (GetFuncIdx y)

        | { ty = t; init = y ; mode = elemmode.Declarative } when OnlyIdx y ->
            &0x03 => GenRefType t => elmkind => GenVec Idx (GetFuncIdx y)

        | { ty = ty; init = el; mode = elemmode.Passive}  -> 
            &0x05 => GenRefType ty => GenVec GenExpr el

        | { ty = ty; init = el ; mode = elemmode.Active frame } ->
            &0x06 => Idx frame.index => GenExpr frame.offset => GenRefType ty => GenVec GenExpr el

        | { ty = ty; init = el ; mode = elemmode.Declarative } ->
            &0x07 => GenRefType ty => GenVec GenExpr el

         
    let ElementSection elms = Section 0x09 (GenVec GenElement) elms


    let GenFunc ({ ty = i; locals = locals; body = e } : func<_,_,_,_>) = 
        let local (n, ty) = Idx n => GenValueType ty 
        let func = GenVec local locals => GenExpr e  
        Idx (Seq.length func) => func


    let CodeSection fs = Section 0x0A (GenVec GenFunc) fs


    let GenData : data<'id, 'fid, 'mem, 'info> -> byte seq = function 
        | { init = b; mode = Active mode } when mode.memory = 0 -> &0x00 => GenExpr mode.offset => Idx b.Length => b
        | { init = b; mode = Passive } -> &0x01 => Idx b.Length => b
        | { init = b; mode = Active mode } -> &0x02 => Idx mode.memory => GenExpr mode.offset => Idx b.Length => b


    let DataSection data = Section 0x0B (GenVec GenData) data


    let DataCountSection (data: seq<data<_,_,_, _>>) = 
        if Seq.isEmpty data then
            seq[]
        else
            &0x0C => u32 (Seq.length data)


    let magic = List.map byte [0x00; 0x61; 0x73; 0x6D]
    let version = List.map byte [0x01; 0x00; 0x00; 0x00]

    let GenModule 
            {
                types       = ty  
                funcs       = fs   
                tables      = tt  
                memories    = ms
                globals     = gs 
                elements    = es
                datas       = ds   
                imports     = ims 
                export      = exs  
                start       = start
    
            } = 
        magic
        => version 
        => TypeSection ty
        => ImportSection ims
        => FunctionSection fs
        => TableSection tt
        => MemorySection ms
        => GlobalSection gs
        => ExportSection exs
        => StartSection start
        => ElementSection es
        => DataCountSection ds
        => CodeSection fs
        => DataSection ds
    

    open System.IO

    let WriteExe (path: string) program =
        let file = 
            if path.EndsWith(".wasm") then
                File.Create path
            else
                File.Create (path + ".wasm")

        GenModule program
        |> Seq.iter (fun b -> file.WriteByte b)
        file.Flush()
        file.Dispose()