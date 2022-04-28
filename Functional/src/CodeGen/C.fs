namespace CodeGen

//#nowarn "25"
(*
    no warning on
        25 - missing cases (checked by invariant)


    Assumptions:
        We do not include instructions for manage memory, 
        but simply add a nametag to each location
        This is used in optimizezation like, removing reduntent load/store instructions
        We do not assign phiscal addresses of local declared variables in function yet.
        
    TODO:
        1) implement arrays, records and tagged unions
        2) implement memory management 
        3) implement a way to translate into a wasm module
*)

open SymTab

open Helpers

open AbstractSyntax.Wasm
open Wasm
open Values
open Types
open MemoryManagement

#nowarn "64"

module C =
   

    open AbstractSyntax.C
    
    let IsIn s item = (Set.contains << Set.ofSeq) s item

    let rec private TypeOf : _ -> Type = 
        function
        | I8  _     -> I8()
        | I16 _     -> I16()
        | I32 _     -> I32()
        | I64 _     -> I64()
        | U8  _     -> U8()
        | U16 _     -> U16()
        | U32 _     -> U32()
        | U64 _     -> U64()
        | F32 _     -> F32()
        | F64 _     -> F64()



    let ( => ) s1 s2 = Sequence(s1, s2)

    // This values are used to easily change pointer types in the code below
    // to change type just change "pointer op"
    let inline pointerop v = U64 v
    let pointertype = pointerop() : Type
    let pointersize = SizeOf pointertype
    
    // to simplify readability and coding
    let (-->) stmt1 stmt2 = Sequence(stmt1, stmt2)

    let inline i32 v = Ok (op.I32(int v))
    let inline i64 v = Ok (op.I64(int64 v))
    let inline u32 v = Ok (op.U32(uint v))
    let inline u64 v = Ok (op.U64(uint64 v))
    let inline f32 v = Ok (op.F32(float32 v)) 
    let inline f64 v = Ok (op.F64(float v)) 


    let inline GenZero v = 
        match v with
        | I8 _ | I16 _ | I32 _ -> Result.map (fun v' -> &Const (v', TypeOf v)) (i32 0) 
        | U8 _ | U16 _ | U32 _ -> Result.map (fun v' -> &Const (v', TypeOf v)) (u32 0)
        | I64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (i64 0)
        | U64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (u64 0)
        | F32 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (f32 0)
        | F64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (f64 0)
        


    let inline GenOne v = 
        match v with
        | I8 _ | I16 _ | I32 _ -> Result.map (fun v' -> &Const (v', TypeOf v)) (i32 1) 
        | U8 _ | U16 _ | U32 _ -> Result.map (fun v' -> &Const (v', TypeOf v)) (u32 1)
        | I64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (i64 1)
        | U64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (u64 1)
        | F32 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (f32 1)
        | F64 _                -> Result.map (fun v' -> &Const (v', TypeOf v)) (f64 1)


    let GenValue = function
        | I8  v -> i32 v
        | I16 v -> i32 v
        | I32 v -> i32 v
        | I64 v -> i64 v
        | U8  v -> u32 v
        | U16 v -> u32 v
        | U32 v -> u32 v
        | U64 v -> u64 v
        | F32 v -> f32 v
        | F64 v -> f64 v


    let GenType = function
        | I8 _  | I16 _ | I32 _     
        | U8 _  | U16 _ | U32 _  -> I32_t 
        | I64 _ | U64 _ -> I64_t
        | F32 _ -> F32_t
        | F64 _ -> F64_t



    let GenTypes tys = List.map GenType tys


    let GetOperand = function
        | I8 _    
        | I16 _  
        | I32 _   -> Integer op.I32
        | I64 _   -> Integer op.I64
        | U8 _    
        | U16 _    
        | U32 _ -> Integer op.U32
        | U64 _  -> Integer op.U64
        | F32 _ -> Float op.F32
        | F64 _ -> Float op.F64




    let GenIntBinOp = function
        | binOp.Add           -> Ok IntOp.binop.Add
        | binOp.Sub           -> Ok IntOp.binop.Sub
        | binOp.Div           -> Ok IntOp.binop.Div
        | binOp.Mul           -> Ok IntOp.binop.Mul
        | binOp.Rem           -> Ok IntOp.binop.Rem
        | binOp.And           -> Ok IntOp.binop.And
        | binOp.Or            -> Ok IntOp.binop.Or
        | binOp.Xor           -> Ok IntOp.binop.Xor
        | binOp.Rotl          -> Ok IntOp.binop.Rotl
        | binOp.Rotr          -> Ok IntOp.binop.Rotr
        | binOp.LeftShift     -> Ok IntOp.binop.Shl 
        | binOp.RightShift    -> Ok IntOp.binop.Shr
        | op -> Error $"{op} is not defined on integers"



    let GenFloatBinOp = function
        | binOp.Add           -> Ok FloatOp.binop.Add
        | binOp.Sub           -> Ok FloatOp.binop.Sub
        | binOp.Div           -> Ok FloatOp.binop.Div
        | binOp.Mul           -> Ok FloatOp.binop.Mul
        | binOp.Min           -> Ok FloatOp.binop.Min
        | binOp.Max           -> Ok FloatOp.binop.Max
        | binOp.CopySign      -> Ok FloatOp.binop.CopySign
        | op -> Error $"{op} is not defined on floating point number"


    let GenIntRelOp = function
        | Eq        -> Ok IntOp.relop.Eq
        | Nq        -> Ok IntOp.relop.Ne
        | Greater   -> Ok IntOp.relop.Gt
        | Geq       -> Ok IntOp.relop.Ge
        | Less      -> Ok IntOp.relop.Lt
        | Leq       -> Ok IntOp.relop.Le


    let GenFloatUnOp = function
        | Neg       -> Ok FloatOp.unop.Neg
        | Abs       -> Ok FloatOp.unop.Abs
        | Ceil      -> Ok FloatOp.unop.Ceil
        | Floor     -> Ok FloatOp.unop.Floor
        | Trunc     -> Ok FloatOp.unop.Trunc
        | Nearest   -> Ok FloatOp.unop.Nearest
        | Sqrt      -> Ok FloatOp.unop.Sqrt


    let GenFloatRelOp = function
        | Eq        -> Ok FloatOp.relop.Eq
        | Nq        -> Ok FloatOp.relop.Ne
        | Greater   -> Ok FloatOp.relop.Gt
        | Geq       -> Ok FloatOp.relop.Ge
        | Less      -> Ok FloatOp.relop.Lt
        | Leq       -> Ok FloatOp.relop.Le

    let GenSign (t: Type) = 
        match t with
        | F32()  | F64()                 -> None
        | I8()   | I16() | I32() | I64() -> Some SX
        | _                              -> Some ZX

    let GenPackSize = function
        | I8()  | U8()  -> Some Pack8
        | I16() | U16() -> Some Pack16
        | I32() | U32() -> Some Pack32
        | _                  -> None

    let rec GenMemFormat ty = 
        GenPackSize ty
        |> Option.map 
            (fun sz ->
                GenSign ty
                |> Option.map (fun sign ->
                    sz, sign
                )
            )
        |> Option.flatten
        
    and GetType vtab e =
        match e with
        | Val v -> Ok(TypeOf v)
        | Var name -> 
            LookUp vtab name
            |> Result.map (fun (t,_) -> t)

        | Convert e -> Ok(TypeOf e) // handy reuse since we assume that the underlying type are correct
        | Binop(t,_,_,_) -> Ok t
        | Unop(t,_,_) -> Ok t
        | Expr.If(t,_,_,_) -> Ok t
        | Cond _ -> Ok(I32())
        | Expr.Call(t,_,_) -> Ok(t)

    and GenExpr offset vtab ftab = function
        | Val v -> GenValue v |> Result.map (fun v' -> &Const(v', TypeOf v))

        | Var name -> 
            match LookUp vtab name with
            | Error msg -> Error msg
            | Ok(ty, (Memory _ as loc)) -> 
                // if location resides in memory, we need to load the current frame pointer as the address
                // since the addres of the value is relative to that.
                Ok(&Load(Global CurrentFrame, I32())) <+> Ok(&Load(loc,ty)) 

            | Ok(ty, loc) ->  Ok(&Load(loc,ty))

        //| IndexOf(index) -> GenIndex offset vtab ftab index


        | Convert e' ->
            match e' with
            | I8 e -> 
                let code = GenExpr offset vtab ftab e  
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _  | U8  _ -> code // do nothing
                    | I16 _ | I32 _ 
                    | U16 _ | U32 _ -> code <+ Const(op.I32 0xFF, U16()) <+ Binary(op.I32 IntOp.And, TypeOf e') 
                    | I64 _ 
                    | U64 _ -> code <+ instr.Convert(op.I64 IntOp.WrapI64, I32()) <+ Const(op.I32 0x807F, I32()) <+ Binary(op.I32 IntOp.And, TypeOf e')
                    | F32 _ -> code <+ instr.Spec(op.I32 IntOp.TruncSatF32, I32()) <+ Const(op.I32 0x807F, I32()) <+ Binary(op.I32 IntOp.And, TypeOf e') 
                    | F64 _ -> code <+ instr.Spec(op.I32 IntOp.TruncSatF64, I32()) <+ Const(op.I32 0x807F, I32()) <+ Binary(op.I32 IntOp.And, TypeOf e') 
                    | _ -> Error "Type Mismatch"

            | I16 e | U16 e ->
                let code = GenExpr offset vtab ftab e  
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _ | U8 _   -> code // do nothing
                                            // 0x807F is the the binary value with the most significant bit
                                            // and the 7 least significant bits set 
                    | I16 _ | I32 _ 
                    | U16 _ | U32 _ -> code <+ Const(op.I32 0xFFFF, U16()) <+ Binary(op.I32 IntOp.And, U16()) 
                    | I64 _ 
                    | U64 _ -> code <+ instr.Convert(op.I64 IntOp.WrapI64, I32()) <+ Const(op.I32 0x807FFF, U16()) <+ Binary(op.I32 IntOp.And, TypeOf e')
                    | F32 _ -> code <+ instr.Spec(op.I32 IntOp.TruncSatF32, I32()) <+ Const(op.I32 0x807FFF, I16()) <+ Binary(op.I32 IntOp.And, TypeOf e') 
                    | F64 _ -> code <+ instr.Spec(op.I32 IntOp.TruncSatF64, I32()) <+ Const(op.I32 0x807FFF, I16()) <+ Binary(op.I32 IntOp.And, TypeOf e') 
                    | _ -> Error "Type Mismatch"

            | I32 e | U32 e  ->
                let code = GenExpr offset vtab ftab e  
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _  | U8 _   
                    | I16 _ | I32 _ 
                    | U16 _ | U32 _ -> code 
                    | I64 _ 
                    | U64 _     -> code <+ instr.Convert(op.I32 IntOp.WrapI64,  TypeOf e') 
                    | F32 _     -> code <+ instr.Spec(op.I32 IntOp.TruncSatF32, TypeOf e')
                    | F64 _     -> code <+ instr.Spec(op.I32 IntOp.TruncSatF64, TypeOf e')
                    | _         -> Error "Type Mismatch"

            | I64 e | U64 e ->
                let code = GenExpr offset vtab ftab e  
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _  | U8 _   
                    | I16 _ | I32 _ 
                    | U16 _ | U32 _ -> code <+ instr.Convert(op.I64 IntOp.Extend,  TypeOf e') 
                    | I64 _ 
                    | U64 _     -> code 
                    | F32 _     -> code <+ instr.Spec(op.I64 IntOp.TruncSatF32, TypeOf e')
                    | F64 _     -> code <+ instr.Spec(op.I64 IntOp.TruncSatF64, TypeOf e')
                    | _         -> Error "Type Mismatch"

            | F32 e ->
                let code = GenExpr offset vtab ftab e
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _ | I16 _ | I32 _ -> code <+ instr.Convert(op.F32 FloatOp.ConvertSI32, TypeOf e')
                    | U8 _ | U16 _ | U32 _ -> code <+ instr.Convert(op.F32 FloatOp.ConvertUI32, TypeOf e')
                    | I64 _ -> code <+ instr.Convert(op.F32 FloatOp.ConvertSI64, TypeOf e')
                    | U64 _ -> code <+ instr.Convert(op.F32 FloatOp.ConvertUI64, TypeOf e')
                    | F32 _ -> code
                    | F64 _ -> code <+ instr.Convert(op.F32 FloatOp.DemoteF64, TypeOf e')
                    | _         -> Error "Type Mismatch"
            
            | F64 e ->
                let code = GenExpr offset vtab ftab e
                match GetType vtab e with
                | Error msg -> Error msg
                | Ok t -> 
                    match t with
                    | I8 _ | I16 _ | I32 _ -> code <+ instr.Convert(op.F64 FloatOp.ConvertSI32, TypeOf e')
                    | U8 _ | U16 _ | U32 _ -> code <+ instr.Convert(op.F64 FloatOp.ConvertUI32, TypeOf e')
                    | I64 _ -> code <+ instr.Convert(op.F64 FloatOp.ConvertSI64, TypeOf e')
                    | U64 _ -> code <+ instr.Convert(op.F64 FloatOp.ConvertUI64, TypeOf e')
                    | F32 _ -> code <+ instr.Convert(op.F64 FloatOp.PromoteF32, TypeOf e')
                    | F64 _ -> code 
                    | _         -> Error "Type Mismatch"

            //| _         -> Error "Type Mismatch"

        | Binop (ty, op', left, right) as e ->
            let correct =
                match ty with
                | I8 _  | U8 _  when Set.contains op' (set[binOp.Add; binOp.Sub; binOp.Mul]) -> Ok(&Const(op.I32 0xFF, ty))   <+> Ok(&Binary(op.I32 IntOp.And, ty))
                | I16 _ | U16 _ when Set.contains op' (set[binOp.Add; binOp.Sub; binOp.Mul]) -> Ok(&Const(op.I32 0xFFFF, ty)) <+> Ok(&Binary(op.I32 IntOp.And, ty))
                
                | _ -> Ok(seq[])

            let lcode = GenExpr offset vtab ftab left
            let rcode = GenExpr offset vtab ftab right
            let op = 
                match GetOperand ty with
                | Integer operand -> Result.map operand (GenIntBinOp op')
                | Float operand -> Result.map operand (GenFloatBinOp op')
                |> Result.map 
                    (fun op -> 
                        &Binary(op, ty))
                        
            lcode <+> rcode <+> op <+> correct
        
        | Unop(ty, op, e) ->
            let code = GenExpr offset vtab ftab e
            match GetOperand ty with
            | Float operand -> 
                Result.map operand (GenFloatUnOp op)
                |> fun op -> code <+> Result.map(fun op -> &Unary(op, ty)) op

            // the only integer unary operation are negation
            | Integer op -> GenZero ty <+> code <+> Ok(&Binary(op IntOp.binop.Sub,ty))

        | True  -> GenOne (I32())
        | False -> GenZero (I32())
        | Relop(ty, op, left, right) ->
            let lcode = GenExpr offset vtab ftab left
            let rcode = GenExpr offset vtab ftab right
            let op =
                match GetOperand ty with
                | Integer operand    -> Result.map operand (GenIntRelOp op)
                | Float operand -> Result.map operand (GenFloatRelOp op)
                |> Result.map (fun op -> &Compare(op, I32()))
            lcode <+> rcode <+> op

        | Expr.If(ty, cond, tb, fb) ->
            let ccode = GenExpr offset vtab ftab cond
            let tcode = GenExpr offset vtab ftab tb
            let fcode = GenExpr offset vtab ftab fb
            let ty' = GenType ty |> NumType
            match tcode, fcode with
            | Error msg, _ | _, Error msg -> Error msg
            | Ok tcode, Ok fcode -> ccode <+ instr.If(ValBlockType(Some(ty')),tcode,fcode, ty)

            
        // simple function call
        | Expr.Call(ty, name, args) ->
            let initcode = 
                List.map (GenExpr offset vtab ftab) args
                // executed in reverse order to make the correct stack layout
                |> List.fold (fun code arg -> arg <+> code) (Ok(seq[]))

            
            // set the caller frame startpoint into the stack
            // set the callee frame start into current frame pointer
            // update stack pointer to point after that pointer
            let (_, vtab), setframe =
                Dec(pointertype, "tmp stack pointer", Var CurrentFrame)                                                // push current frame top pointer to the stack. 
                --> Assign(CurrentFrame, Var StackPointer)                                                              // set new frame pointer into current fram pointer
                --> Assign(StackPointer, Binop(pointertype, binOp.Add, Var StackPointer, Val(pointerop (uint64 pointersize))))   // update stackpointer
                |> GenStatement 0 0 vtab ftab                                                                          // we do not care about nesting since this is a 

            // set stack pointer to curremtframe start point.
            // get the caller frame startpoint from the stack and put it into current frame pointer
            let _, endframe =
                Assign(StackPointer, Var CurrentFrame)
                --> Assign(CurrentFrame, Var "tmp stack pointer")
                |> GenStatement 0 0 vtab ftab

            match LookUp ftab name with
            | Error msg    -> Error msg
            | Ok ty ->
                // assumes that the old stack pointer are on the top of the stack on return
                initcode <+> setframe <+ instr.Call(name, ty) <+> endframe


   
    
    and GenStatement offset nesting vtab ftab = function
        | Dec(ty, name, body) ->
            let loc = Memory {ty = GenType ty; name = name; align = 2; offset = offset; sz = GenMemFormat ty }
            let vtab = Bind vtab name (ty, loc)
            let code = GenExpr offset vtab ftab body <+> Ok(&Load(Global CurrentFrame, pointertype)) <+> Ok(&Store(loc, ty))
            (offset + SizeOf ty, vtab), code 
            // need to add heap management to take care of declareation of arrays

//        | ToIndex(Index(id, _) as index, expr) -> 
//            match LookUp vtab id with
//            | Error msg -> (offset, vtab), Error msg
//            | Ok(ty, loc) ->
//                let adr = GenIndex offset vtab ftab index
//                let value = GenExpr offset vtab ftab expr
//                (offset, vtab), adr <+> value <+ Store(loc, []) 

        | Assign(name, body) ->
            match LookUp vtab name with
            | Error msg -> (offset, vtab), Error msg
            | Ok (ty, store) ->
                let code = GenExpr offset vtab ftab body <+> Ok(&Store(store, ty))
                (offset, vtab), code 

        //| Expr e -> (offset, vtab), GenExpr offset vtab ftab e

        | If(cond, tbranch, fbranch) ->
            let ccode = GenExpr offset vtab ftab cond
            let icode =
                GenStatement offset nesting vtab ftab tbranch
                |> snd
                |> Result.map (fun tcode ->
                    match fbranch with
                    | None -> Ok(seq[])
                    | Some fbranch ->
                        GenStatement offset nesting vtab ftab fbranch
                        |> snd
                        |> Result.map(fun fcode ->
                                &instr.If(ValBlockType None, tcode, fcode, Unit)
                            )
                        )
                |> Result.Collaps

            (offset, vtab), ccode <+> icode

        | While(cond, body, ty) ->
            (*
                nesting start at count 0
                when generating the body we increment the nesting by 2 (a blokc and a loop)
            *)
            let ty = match ty with None -> Unit | Some t -> t
            let ccode = GenExpr offset vtab ftab cond
            let _, bcode = GenStatement offset (nesting + 2) vtab ftab body

        
            Result.map (fun bcode ->
                    &Block( // label nesting
                        ValBlockType None, 
                        &Loop( // label nesting + 1
                            ValBlockType None, 
                            Seq.append bcode &(BrIf($"{nesting+1}",ty)) |> List.ofSeq
                            ,ty
                        ),
                        ty
                   )
            ) (bcode <+> ccode) 
            |> fun icode -> (offset, vtab), ccode <+> icode

             
        

        // when generating break and continues we assume they are in
        // a loop hence we have incremented the nesting by 2
        // break = jump to the end of the block (exiting the loop)
        // continue = jump to the start of the loop (continue the loop)
        | Break -> 
            if nesting = 0 then
                (offset, vtab), Error "Not in a loop"
            else
                (offset, vtab), Ok(&Br($"{nesting - 2}", Unit))

        | Continue -> 
            if nesting = 0 then
                (offset, vtab), Error "Not in a loop"
            else
                (offset, vtab), Ok(&Br($"{nesting - 1}", Unit))
             
        | Return -> 
            (offset, vtab), Ok(&instr.Return(Unit))
        
        | Expr e -> 
            (offset, vtab), GenExpr offset vtab ftab e
        
        | Sequence(stmt1, stmt2) ->
            let (offset, vtab), code1 = GenStatement offset nesting vtab ftab stmt1
            let (offset, vtab), code2 = GenStatement offset nesting vtab ftab stmt2
            (offset, vtab), code1 <+> code2




    and GenFunctionType (Fun(_, args, ty, _)) =
        let locals = List.map (NumType << GenType << snd) args
        FuncType(Result locals, Result [NumType (GenType ty)])



    and GenFunction vtab ftab (Fun(fname, args, ty, body) as f) =
        let vtab = List.fold (fun tab (argname, argtype) -> Bind tab argname (argtype, Local argname)) vtab args
        let ftab = Bind ftab fname ty

        let (FuncType(Result locals,_) as ft) = GenFunctionType f
        let locals = List.zip [ for i in 0 .. locals.Length - 1  -> Local (string i) ] locals
        
        // Assumption by invariance, that all function calls stackframe start with the pointer to the start of the last frame
        match GenStatement pointersize 0 vtab ftab body with
        | _, Error msg -> Error msg
        | _, Ok(body) -> Ok(ft, { ty = -1; name = fname; locals = locals; body = expr.Expr (List.ofSeq body) })
    

    and GenFunctions vtab funs =
        let GetFunc (Fun(fname, _, ty, _)) = fname, ty
        let ftab = 
            funs
            |> List.map GetFunc
            |> List.fold (fun tab (name, ty) -> Bind tab name ty) (DynamicTable<_,_>.Empty() :> _)

        funs
        |> List.map (GenFunction vtab ftab)
        |> List.fold (fun funs fun' -> 
            Result.map2 (fun funs fun' -> fun' :: funs) funs fun') (Ok [])


    and GenGlobals offset vtab ftab (Globals globals) =
            // generate wasm globals binary format
            List.fold 
                (fun globals (id, ty, init) -> 
                    Result.map (fun init ->
                    id, 
                    { ty = GlobalType(NumType (GenType ty), Mutable); name = ""; init = expr.Expr (init |> List.ofSeq)}
                    ) (GenExpr offset vtab ftab init) :: globals
                ) [] globals
            // Carry the list into the Result type and propergate errors out of the list
            |> List.fold (fun globals glob -> 
                match glob with
                | Error msg -> 
                    match globals with
                    | Error msg' ->  Error $"{msg'}\n{msg}"
                    | _ -> Error msg

                | Ok(id, glob) -> Result.map (fun globals -> (id, glob) :: globals) globals
                ) (Ok([]))

    and GenData data =
        List.map (fun (id, datapoint) ->
            id, { init = Data.ToBytes datapoint; mode = datamode.Passive }
        ) data     


    and GenProgram (Enviroment((Globals globals as globals'), data, Funs program)) =
        (*
            TODO: Convert function, global, local and import ids to numerical ids
        *)
        let vtab = 
            List.fold 
                (fun tab (id, ty, _) ->
                    Bind tab id (ty, Global id)
                ) (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>) globals
             
        let ftab = DynamicTable<_,_>.Empty() :> ISymbolic<_,_>

        let funs = 
            GenFunctions vtab program
            |> Result.map (fun funs -> List.unzip funs)
           
        let dataids, data = GenData data |> List.unzip

        GenGlobals 0 vtab ftab globals' 
        |> Result.map (fun globals -> 
            { Module<_,_,_,_>.empty() with globals = seq (List.map snd globals) }
        )
        |> Result.map2 (fun (ftype, funs) module' -> 
            { module' with
                types = ftype
                funcs = funs
                datas = data
            }
        ) funs

        
        
            