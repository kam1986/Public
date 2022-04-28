namespace Wat

open AbstractSyntax.Wasm
open Wasm
open Types
open Values
open Helpers

module Encoding =

    let PrintValType = function
        | I32_t -> "i32"
        | I64_t -> "i64"
        | F32_t -> "f32"
        | F64_t -> "f64"

    let PrintOp = function
        | I32 _ -> "i32."
        | I64 _ -> "i64."
        | U32 _ -> "i32."
        | U64 _ -> "i64."
        | F32 _ -> "f32."
        | F64 _ -> "f64."
    
    let PrintValue = function
        | I32 v -> string v
        | I64 v -> string v
        | U32 v -> string v
        | U64 v -> string v
        | F32 v -> string v
        | F64 v -> string v


    let PrintSign = function
        | I32 _ | I64 _ -> "_s"
        | U32 _ | U64 _ -> "_u"
        | F32 _ -> ""
        | F64 _ -> ""

    let PrintUnOp unop = 
        match unop with
        | I32 op 
        | U32 op
        | I64 op
        | U64 op ->
            match op with
            | IntOp.Clz    -> "clz"
            | IntOp.Ctz    -> "ctz"
            | IntOp.Popcnt -> "popcnt"
        | F32 op 
        | F64 op ->
            match op with
            | FloatOp.Neg       -> "neg"
            | FloatOp.Abs       -> "abs"
            | FloatOp.Ceil      -> "ceil"
            | FloatOp.Floor     -> "floor"
            | FloatOp.Trunc     -> "trunc"
            | FloatOp.Nearest   -> "nearest"
            | FloatOp.Sqrt      -> "sqrt"
        |> fun op -> PrintOp unop + op // prepend type info i.e i32., i64., f32., f64.


    let PrintBinOp binop = 
        match binop with
        | I32 op | I64 op | U32 op | U64 op ->
            match op with
            | IntOp.Add  -> "add"  
            | IntOp.Sub  -> "sub"
            | IntOp.Mul  -> "mul" 
            | IntOp.Div  -> "div" + PrintSign binop
            | IntOp.Rem  -> "rem" + PrintSign binop
            | IntOp.And  -> "and"
            | IntOp.Or   -> "or"
            | IntOp.Xor  -> "xor"
            | IntOp.Shl  -> "shl"
            | IntOp.Shr  -> "shr" + PrintSign binop
            | IntOp.Rotl -> "rotl"
            | IntOp.Rotr -> "rotr"

        | F32 op | F64 op ->
            match op with
            | FloatOp.Add      -> "add"
            | FloatOp.Sub      -> "sub"
            | FloatOp.Mul      -> "mul" 
            | FloatOp.Div      -> "div"
            | FloatOp.Min      -> "min"
            | FloatOp.Max      -> "max"
            | FloatOp.CopySign -> "copysign"
        |> fun op -> PrintOp binop + op // prepend type info i.e i32., i64., f32., f64.
    
    let PrintTestOp testop = 
        match testop with
        | I32 op | I64 op | U32 op | U64 op -> "eqz"
        | _ -> "testop_error"
        |> fun op -> PrintOp testop + op // prepend type info i.e i32., i64., f32., f64.

    let PrintRelOp relop =
        match relop with
        | I32 op | I64 op | U32 op | U64 op ->
            match op with
            | IntOp.Eq -> "eq"
            | IntOp.Ne -> "nq"
            | IntOp.Lt -> "lt" + PrintSign relop
            | IntOp.Gt -> "gt" + PrintSign relop
            | IntOp.Le -> "le" + PrintSign relop
            | IntOp.Ge -> "ge" + PrintSign relop
        | F32 op | F64 op ->
            match op with
            | FloatOp.Eq -> "eq"
            | FloatOp.Ne -> "nq"
            | FloatOp.Lt -> "lt"
            | FloatOp.Gt -> "gt"
            | FloatOp.Le -> "le"
            | FloatOp.Ge -> "ge"
        |> fun op -> PrintOp relop + op // prepend type info i.e i32., i64., f32., f64.

    
    let PrintCvtOp cvtop = 
        match cvtop with
        | I32 op | I64 op | U32 op | U64 op ->
            match op with
            | IntOp.WrapI64  -> "wrap64" 
            | IntOp.TruncF32 -> "trunc_f32"
            | IntOp.TruncF64 -> "trunc_f64"
            | IntOp.Extend   -> "extend_i32"
        | F32 op | F64 op ->
            match op with
            | FloatOp.ConvertSI32 -> "convert_i32_s"
            | FloatOp.ConvertUI32 -> "convert_i32_u"
            | FloatOp.ConvertSI64 -> "convert_i64_s"
            | FloatOp.ConvertUI64 -> "convert_i64_u"
            | FloatOp.DemoteF64   -> "demote_f64"
            | FloatOp.PromoteF32  -> "promote_f32"
        |> fun op -> PrintOp cvtop + op // prepend type info i.e i32., i64., f32., f64.


    let PrintSpecOp specop =
        match specop with
        | I32 op 
        | U32 op ->
            match op with
            | IntOp.ExtendS packSize -> 
                match packSize with
                | Pack8 -> "8_S"
                | Pack16 -> "16_s"
                | Pack32 -> "32_s"
                |> fun p -> "extend" + p

            | IntOp.TruncSatF32      -> "trunc_sat_f32"
            | IntOp.TruncSatF64      -> "trunc_sat_f64"
            | IntOp.Reinterpret      -> "reinterpret_" + PrintValType F32_t
                
        | I64 op 
        | U64 op ->
            match op with
            | IntOp.ExtendS packSize -> 
                match packSize with
                | Pack8 -> "8_S"
                | Pack16 -> "16_s"
                | Pack32 -> "32_s"
                |> fun p -> "extend" + p

            | IntOp.TruncSatF32      -> "trunc_sat_f32"
            | IntOp.TruncSatF64      -> "trunc_sat_f64"
            | IntOp.Reinterpret      -> "reinterpret_" + PrintValType F64_t
        
        | F32 _ -> "reinterpret_" + PrintValType I32_t
        | F64 _ -> "reinterpret_" + PrintValType I64_t
        |> fun op -> PrintOp specop + op // prepend type info i.e i32., i64., f32., f64.

    let PrintLimits = function
        | { min = n; max = None } -> 
            "{min " + string n + ", " + " max}"

        | { min = n; max = Some m } -> 
            "{min " + string n + ", " + " max " + string m + "}"


    let PrintVec (printer : 'a -> string) vec = 
        Seq.map printer vec
        |> Seq.fold (fun code c -> $"{code}{c}") ""


    let PrintRefType = function
        | refType.FuncRef -> "funcref"
        | _ -> "externref"

    let PrintValueType = function
        | NumType t -> PrintValType t
        | RefType t -> PrintRefType t

    let Print r ty = $"({r} {PrintValueType ty})"
   

    let PrintParam = Print "param"
    
    let PrintResult = Print "result"

    let PrintType ty = $"(type {ty})"

    let PrintFuncType (FuncType(Result prms, Result rets)) =
        $"func {PrintVec PrintParam prms} {PrintVec PrintResult rets}"

    
    let PrintMemType (MemoryType limits) = PrintLimits limits


    let PrintTableType (TableType(limits, rt)) =
        $"{PrintLimits limits} {PrintRefType rt}"


    let PrintGlobalType (GlobalType(vt, mut)) =
        match mut with
        | Immutable -> PrintValueType vt
        | Mutable -> $"(mut {PrintValueType vt})"


    let PrintBlockType = function
        | ValBlockType (Some t) -> $"{PrintResult t}"
        | VarBlockType (idx) -> $"({PrintType idx})" // 
        | _ -> ""

    let Indentation n =
        List.map (fun _ -> "  ") [ 0 .. n - 1]
        |> List.fold (fun a b -> $"{a}{b}") ""


    let PrintExtention = function SX -> "_s" | ZX -> "_u"


    let PrintLoad { ty = ty; align = align; name = _; offset = offset; sz = sz } =
        let ty = PrintValType ty
        let pack =
            Option.map 
                (function
                    | Pack8, e -> $"8{PrintExtention e}"
                    | Pack16, e -> $"16{PrintExtention e}"
                    | Pack32, e when ty <> "i32" ->  $"32{PrintExtention e}"
                    | _ -> ""                    
                ) sz
            |> function None -> "" | Some p -> p

        let align = if align = 0 then "" else $"align={align}"
        let offset = if offset = 0 then "" else $"offset={offset}"

        $"{ty}.load{pack} {offset} {align}"
        

    let PrintStore { ty = ty; align = align; name = _; offset = offset; sz = sz } =
        let ty = PrintValType ty
        let pack =
            Option.map 
                (function
                    | Pack8, _ -> $"8"
                    | Pack16, _ -> $"16"
                    | Pack32, _ when ty <> "i32" ->  $"32"
                    | _ -> ""
                ) sz
            |> function None -> "" | Some p -> p

        let align = if align = 0 then "" else $"align={align}"
        let offset = if offset = 0 then "" else $"offset={offset}"

        $"{ty}.store{pack} {offset} {align}"

    // print a list of string representation of instructions
    // allows for adding byte count 
    let rec PrintInstruction = function
    | Unreachable _     -> ["unreachable"]
    | Nop _             -> ["nop"]
    | Br(label, _)      -> [$"br {label}"]
    | BrIf(label, _)    -> [$"br_if {label}"]
    | BrTable(labels, defaultlabel, _) -> 
        let labels = List.fold (fun a b -> $"{a} {b}") "" labels
        [$"br_table {labels} {defaultlabel}"]

    | Return _ -> ["return"]
    | Call(func, _) -> [$"call {func}"]
    | CallIndirect(tableidx, elmidx, _) -> [$"call_indirect {tableidx} {elmidx}"]
    | Block(bt, body, _) -> 
        let bt = PrintBlockType bt
        let body = 
            Seq.map (fun instr -> PrintInstruction instr) body
            |> Seq.concat
            |> List.ofSeq

        "block" :: bt :: body @ ["end"]

    | Loop(bt, body, _) ->
        let body = 
            Seq.map (fun instr -> PrintInstruction instr) body
            |> Seq.concat
            |> List.ofSeq

        let bt = PrintBlockType bt
        "loop" :: bt :: body @ ["end"]

    | If(bt, tbody, fbody, _) ->
        let bt = PrintBlockType bt
        let tbody =
            Seq.map (fun instr -> PrintInstruction instr) tbody
            |> Seq.concat
            |> List.ofSeq
                

        let fbody =
            Seq.map (fun instr -> PrintInstruction instr) fbody
            |> Seq.concat
            |> List.ofSeq
        
        "if" :: bt :: tbody @ "else" :: fbody @ ["end"]

    | RefNull(rt, _)  -> [$"ref.null {PrintRefType rt}"]
    | RefIsNull _     -> [$"ref.is_null"]
    | RefFunc(func,_) -> [$"ref.func {func}"]
    | Drop(Stack, _)  -> ["drop"] 
    | Select(_, _)    -> [$"select"] // should be corrected when type expantion are made
        
    | Load(Local local, _)  -> [$"local.get {local}"]
    | Store(Local local, _) -> [$"local.set {local}"]
    | Tee(Local local, _)   -> [$"local.tee {local}"]
        
    | Load(Global global', _)  -> [$"global.get {global'}"]
    | Store(Global global', _) -> [$"global.set {global'}"]
    | Tee(Global global', _)   -> $"global.set {global'}" :: [$"global.get {global'}"]
        
    | Load(Table table, _)  -> [$"table.get {table}"]
    | Store(Table table, _) -> [$"table.set {table}"]
    | Tee(Table table, _)   -> $"table.set {table}" :: [$"table.get {table}"]
    | Size(Table table, _)  -> [$"table.size {table}"]
    | Grow(Table table, _)  -> [$"table.grow {table}"]
    | Fill(Table table, _)  -> [$"table.fill {table}"]
    | Copy(Table table1, Table table2, _)  -> [$"table.copy {table1} {table2}"]
    | Init(Table table, Table elm, _)      -> [$"table.init {table} {elm}"]
    | Drop(Table elm, _)    -> [$"table.drop {elm}"]
         
    | Load(Memory memop, _)     -> [PrintLoad memop]
    | Store(Memory memop, _)    -> [PrintStore memop]
    | Size(Memory memop, _)     -> ["memory.size"]
    | Grow(Memory memop, _)     -> ["memory.grow"]
    | Fill(Memory memop, _)     -> ["memory.fill"]
    | Copy(Memory _, _, _)      -> ["memory.copy"]
    | Init(Memory memop, _, _)  -> [$"memory.init {memop.name}"] // name here is the name of the data (dataidx)
    | Drop(Memory memop, _)     -> [$"data.drop {memop.name}"]

    | Const(v, _) -> [PrintOp v + "const " + PrintValue v]
    | Binary(op,_)  -> [PrintBinOp op]
    | Compare(op,_) -> [PrintRelOp op]
    | Unary(op, _)  -> [PrintUnOp op]
    | Spec(op, _) -> [PrintSpecOp op]
    | Test(op, _) -> [PrintTestOp op]
    | _ -> [] // all illegal operations just returns an empty list


    

    

    let rec private GenSExpression instructions = 
        Seq.fold (fun sexprs instruction -> 
            match instruction with
            | Nop info -> Result.map (fun sexprs -> SNop info :: sexprs) sexprs
            | Unreachable info -> Result.map (fun sexprs -> SUnreachable info :: sexprs) sexprs
            | Select(vt, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok (test :: case2 :: case1 :: sexprs) -> Ok(SSelect(vt, test, case1, case2, info) :: sexprs)
                | Ok sexpr -> Error $"Selcet expected 3 arguments but got {sexpr.Length}"
            
            | Block(bt, body, info) -> Result.map2 (fun sexprs body -> SBlock(bt, body, info) :: sexprs) sexprs (GenSExpression body)
            | Loop(bt, body, info)  -> Result.map2 (fun sexprs body -> SLoop(bt, body, info) :: sexprs) sexprs (GenSExpression body)
            | If(bt, tb, fb, info) ->
                let branches = Result.map2 (fun tb fb -> tb,fb) (GenSExpression tb) (GenSExpression fb)
                match sexprs, branches with
                | Error msg, _ | _, Error msg -> Error msg
                | Ok sexprs, Ok (tb, fb) -> Ok(SIf(bt, tb, fb, info) :: sexprs)
                        

            | Br(target, info)    -> Result.map (fun sexprs -> SBr(target, info) :: sexprs) sexprs
            | BrIf(target, info)  -> Result.map (fun sexprs -> SBrIf(target, info) :: sexprs) sexprs
            | BrTable(target1, target2, info) -> Result.map (fun sexprs -> SBrTable(target1, target2, info) :: sexprs) sexprs
            | Return info -> Result.map (fun sexprs -> SReturn info :: sexprs) sexprs
            | Call(target, info) -> Result.map (fun sexprs -> SCall(target, info) :: sexprs) sexprs
            | CallIndirect(target1, target2, info) -> Result.map (fun sexprs -> SCallIndirect(target1, target2, info) :: sexprs) sexprs
            | Load (mem, info) -> Result.map (fun sexprs -> SLoad (mem, info) :: sexprs) sexprs
            | Store(mem, info) -> Result.map (fun sexprs -> SStore(mem, info) :: sexprs) sexprs
            | Tee  (mem, info) -> Result.map (fun sexprs -> STee  (mem, info) :: sexprs) sexprs
            | Grow (mem, info) -> Result.map (fun sexprs -> SGrow (mem, info) :: sexprs) sexprs
            | Size (mem, info) -> Result.map (fun sexprs -> SSize (mem, info) :: sexprs) sexprs
            | Fill (mem, info) -> Result.map (fun sexprs -> SFill (mem, info) :: sexprs) sexprs
            | Copy (mem1, mem2, info) -> Result.map (fun sexprs -> SCopy (mem1, mem2, info) :: sexprs) sexprs
            | Init (mem1, mem2, info) -> Result.map (fun sexprs -> SInit (mem1, mem2, info) :: sexprs) sexprs
            | Drop (mem, info) -> Result.map (fun sexprs -> SDrop (mem, info) :: sexprs) sexprs
            | RefNull (rt, info) -> Result.map (fun sexprs -> SRefNull (rt, info) :: sexprs) sexprs
            | RefFunc (id, info) -> Result.map (fun sexprs -> SRefFunc (id, info) :: sexprs) sexprs
            | RefIsNull info -> Result.map (fun sexprs -> SRefIsNull info :: sexprs) sexprs
            | Const (v, info) -> Result.map (fun sexprs -> SConst(v,info) :: sexprs) sexprs
            | Unary (unop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se :: sexprs) -> Ok(SUnary(unop, se, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintUnOp unop} expects 1 argument but got {sexprs.Length}"
            | Test (tstop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se :: sexprs) -> Ok(STest(tstop, se, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintTestOp tstop} expects 1 argument but got {sexprs.Length}"
            | Convert (cvtop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se :: sexprs) -> Ok(SConvert(cvtop, se, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintCvtOp cvtop} expects 1 argument but got {sexprs.Length}"
            | Spec(spcop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se :: sexprs) -> Ok(SSpec(spcop, se, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintSpecOp spcop} expects 1 argument but got {sexprs.Length}"
            | Compare(cmpop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se1 :: se2 :: sexprs) -> Ok(SCompare(cmpop, se1, se2, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintRelOp cmpop} expects 2 argument but got {sexprs.Length}"
            | Binary(binop, info) ->
                match sexprs with
                | Error msg -> Error msg
                | Ok(se1 :: se2 :: sexprs) -> Ok(SBinary(binop, se1, se2, info) :: sexprs)
                | Ok sexprs -> Error $"{PrintBinOp binop} expects 2 argument but got {sexprs.Length}"
            ) (Ok []) instructions
        |> Result.map (fun sexprs -> Seq sexprs)


    let PrintAsSExpression instructions =
        let rec PrintSExpression i sexpr =
            let indent = Indentation i
            match sexpr with
            | SUnreachable _ -> "unreacable"
            | SNop _    -> "nop"
            | SReturn _ -> "return"
            | SSelect(vts, test, case1, case2, _) ->
                let vts = PrintVec PrintValueType vts
                let test = PrintSExpression (i + 1) test
                let case1 = PrintSExpression (i + 1) case1
                let case2 = PrintSExpression (i + 1) case2
            
                $"{indent}(select {vts}\n{test}\n{case1}\n{case2}\n{indent})"

            | SBlock(bt,body, _) ->
                let bt = PrintBlockType bt
                let body = PrintSExpression (i + 1) body
                $"{indent}(block {bt}\n{body}\n{indent})"

            | SLoop(bt,body, _) ->
                let bt = PrintBlockType bt
                let body = PrintSExpression (i + 1) body
                $"{indent}(loop {bt}\n{body}\n{indent})"
            
            | SIf(bt, truebody, falsebody, _) ->
                let bt = PrintBlockType bt
                let truebody =
                    PrintSExpression (i + 2) truebody
                    |> fun code ->
                        $"\n{indent}  (then\n{code}\n  {indent})"
                let falsebody = 
                    let code = PrintSExpression (i + 2) falsebody 
                    if code = "" then
                        ""
                    else
                        $"\n{indent}  (else\n{code}\n{indent}  )"

                $"{indent}(if {bt}{truebody}{falsebody}\n{indent})"

            | SBr(target, _) -> $"{indent}(br {target})"
            | SBrIf(target, _) -> $"{indent}(br_if {target})"
            | SBrTable(target1, target2, _) -> $"{indent}(br_table {target1} {target2})"
            | SCall(target, _) -> $"{indent}(call {target})"
            | SCallIndirect(target1, target2, _) -> $"{indent}(call_indirect {target1} {target2})"
            | SLoad (Local location, _)  -> $"{indent}(local.get {location})"
            | SStore(Local location, _)  -> $"{indent}(local.set {location})"
            | STee  (Local location, _)  -> $"{indent}(local.tee {location})"
            | SLoad (Global location, _) -> $"{indent}(global.get {location})"
            | SStore(Global location, _) -> $"{indent}(global.set {location})"
            | STee  (Global location, _) -> $"{indent}(global.set {location})\n{indent}(global.get {location})"
            | SLoad (Table location, _)  -> $"{indent}(table.get {location})"
            | SStore(Table location, _)  -> $"{indent}(table.set {location})" 
            | STee  (Table location, _)  -> $"{indent}(table.set {location})\n{indent}(table.get {location})"
            | SGrow (Table location, _)  -> $"{indent}(table.grow {location})"
            | SSize (Table location, _)  -> $"{indent}(table.size {location})"
            | SFill (Table location, _)  -> $"{indent}(table.fill {location})"
            | SDrop (Table location, _)  -> $"{indent}(elem.drop {location})"
            | SCopy (Table location1, Memory location2, _) -> $"{indent}(table.copy {location1} {location2})"
            | SInit (Table location1, Memory location2, _) -> $"{indent}(table.init {location1} {location2})"
            | SLoad(Memory location, _)  -> $"{indent}({PrintLoad location})"
            | SStore(Memory location, _) -> $"{indent}({PrintStore location})"
            | STee(Memory location, _)   -> $"{indent}({PrintStore location})\n{indent}({PrintLoad location})"
            | SGrow(Memory location, _)  -> $"{indent}(memory.grow)"
            | SSize(Memory location, _)  -> $"{indent}(memory.size)"
            | SFill(Memory location, _)  -> $"{indent}(memory.fill)"
            | SDrop(Memory location, _)  -> $"{indent}(data.drop {location.name})"
            | SCopy(Memory location1, Memory location2, _) -> $"{indent}(memory.copy)"
            | SInit(Memory location1, Memory location2, _) -> $"{indent}(memory.init {location1.name})"
            | SRefNull(rt, _) -> $"{indent}(ref.null {PrintRefType rt})"
            | SRefFunc(id, _) -> $"{indent}(ref.func {id})"
            | SRefIsNull _ -> $"{indent}(ref.is_null)"
            | SConst(v,_) -> $"{indent}({PrintOp v}const {PrintValue v})"
            | SUnary(op, body, _)-> $"{indent}({PrintUnOp op}\n{PrintSExpression (i + 1) body}\n{indent})"
            | STest(op, body, _) -> $"{indent}({PrintTestOp op}\n{PrintSExpression (i + 1) body}\n{indent})"
            | SConvert(op, body, _) -> $"{indent}({PrintCvtOp op}\n{PrintSExpression (i + 1) body}\n{indent})"
            | SSpec(op, body, _) -> $"{indent}({PrintSpecOp op}\n{PrintSExpression (i + 1) body}\n{indent})"
            | SCompare(op, left, right, _) -> 
                let left = PrintSExpression (i + 1) left
                let right = PrintSExpression (i + 1) right
                $"{indent}({PrintRelOp op}\n{indent}({left}\n{indent})\n{indent}({right}\n{indent})\n{indent})"

            | SBinary(op, left, right, _) ->
                let left = PrintSExpression (i + 1) left
                let right = PrintSExpression (i + 1) right
                $"{indent}({PrintBinOp op}\n{left}\n{right}\n{indent})"


            | Seq sexprs -> 
                if Seq.isEmpty sexprs then
                    ""
                else
                    Seq.map (PrintSExpression i) sexprs
                    |> Seq.reduce (fun sexprs se -> $"{se}\n{sexprs}") 

            | _ -> $"{indent}error\n" // insert an error mark

        match GenSExpression instructions with
        | Error msg -> msg
        | Ok sexpr -> PrintSExpression 0 sexpr

