
namespace Wasm
(*
    version 1.1
*)

module Validation =
    // the maximum value of any address
    
    let [<Literal>] internal tablesize = System.UInt32.MaxValue
    let [<Literal>] internal memsize = 65536u
    
    
    // the types and abstract syntax tree
    open Types
    open AST

    type Context = {
        
            types: FuncType vec
            funcs: FuncType vec
            tables: TableType vec
            mems: MemType vec
            globals: GlobalType vec
            elems: RefType vec
            datas: Result<unit,string> vec // should be changed
            locals: ValType vec
            labels: ResultType vec
            return': ResultType vec
            refs: funcidx vec
        }

    with
        static member empty =
            {
                types = []
                funcs = []
                tables = []
                mems = []
                globals = []
                elems = []
                datas = []
                locals = []
                labels = []
                return' = []
                refs = []
            }


    // this might be extended in newer versions
    type opcode = LOOP | IF | ELSE | BLOCK 

    // enum of types which are suppresed to use only 8-bits
    type valtype = 
        | I32 = 0y 
        | I64 = 1y 
        | F32 = 2y 
        | F64 = 3y 
        | Funcref = 4y 
        | Externref = 5y

    let typeof tp =
        match tp with
        | RefType FuncRef -> valtype.Funcref
        | RefType _ -> valtype.Externref
        | NumType t ->
            match t with
            | F32 -> valtype.F32
            | F64 -> valtype.F64
            | I64 -> valtype.I64
            | _   -> valtype.I32
        

    let uoptypeof op tp =
        match op with
        | Funop _ ->
            tp = valtype.F32 || tp = valtype.F64
        | Iunop _ ->
            // less computational heavy to do the invert
            tp <> valtype.F32 && tp <> valtype.F64
        |> fun ret ->
            if ret 
            then Ok ()
            else Error "The type of the operand does not comply with the operator."

    let boptypeof op tp =
        match op with
        | Fbinop _ ->
            tp = valtype.F32 || tp = valtype.F64
        | Ibinop _ ->
            // less computational heavy to do the invert
            tp <> valtype.F32 && tp <> valtype.F64
        |> fun ret ->
            if ret 
            then Ok ()
            else Error "The type of the operand does not comply with the operator."

    let roptypeof op tp =
        match op with
        | Frelop _ ->
            tp = valtype.F32 || tp = valtype.F64
        | Irelop _ ->
            // less computational heavy to do the invert
            tp <> valtype.F32 && tp <> valtype.F64
        |> fun ret ->
            if ret 
            then Ok ()
            else Error "The type of the operand does not comply with the operator."

    type ValType = Type of valtype | Unknown

    let isRef t = 
        (t = Type valtype.Funcref || t = Type valtype.Externref || t = Unknown)
        |> fun ret ->
            if ret 
            then Ok () 
            else Error "This is not a reference type"
    
    let isNum t = 
        (t = Type valtype.I32 || t = Type valtype.I64 || t = Type valtype.F32 || t = Type valtype.F64 || t = Unknown)
        |> fun ret ->
            if ret 
            then Ok () 
            else Error "This is not a number type"
    
    let IsInt t =
        (t = Type valtype.I32 || t = Type valtype.I64 || t = Unknown)
        |> fun ret ->
            if ret 
            then Ok () 
            else Error "This is not a integer type"

    type Frame =
        {
            opcode: opcode 
            starttypes: ValType list
            endtypes: ValType list
            height: int
            unreachable: bool
        }

    let pushval item (ctrls,vals) = Ok(ctrls, item :: vals)
    
    let popval0 (ctrls: Frame list, vals: ValType list) =
        if vals.Length = ctrls.[0].height then
            if ctrls.[0].unreachable then
                Error "type error 0"
            else
                Ok(Unknown, (ctrls, vals))
        else
            Ok(List.head vals, (ctrls, List.tail vals))

    let popval expect (ctrls, vals) =
        match popval0 (ctrls,vals) with
        | Ok(Unknown,(ctrls, vals')) -> 
            Ok(expect, (ctrls, vals'))

        | Ok(actual,(ctrls, vals')) when expect = Unknown -> 
            Ok(actual, (ctrls, vals'))
        
        | Ok(actual, _)             when expect <> actual -> 
            Error "type error 1"
                
        | Ok(actual,(ctrls, vals')) -> 
            Ok(actual, (ctrls, vals'))

        | error -> error

    let pushvals vals' (ctrls, vals) = 
        Ok(ctrls, List.fold (fun a b ->  b :: a) vals vals')
    
    // same as appending
    // return (popped, vals)
    let popvals vals' (ctrls, vals) =
        List.fold
            (fun ret value -> 
                match ret with
                | Error msg -> Error msg
                | Ok(popped, vals') ->
                    match popval value (ctrls, vals') with
                    | Error msg -> Error msg
                    | Ok(t, (_, vals')) ->
                        Ok(t::popped, vals')
            ) (Ok(([], vals))) vals'
            |> fun ret -> 
                match ret with
                | Error msg -> Error msg
                | Ok(popped, vals) ->
                    Ok(popped, (ctrls, vals))


    let pushctrl opcode inlst outlst (ctrls, vals: ValType list) =
        let frame = 
            {
                opcode = opcode
                starttypes = inlst
                endtypes = outlst
                height = vals.Length
                unreachable = false
            }
        match pushvals inlst (ctrls, vals) with
        | Error msg -> Error msg
        | Ok(ctrls',vals') ->
            Ok(frame :: ctrls', vals')


    let popctrl (ctrls: Frame list, vals: _ list) =
        if ctrls.IsEmpty then
            Error "type error 2"
        else
            let frame = ctrls.[0]
            match popvals frame.endtypes (ctrls,vals) with
            | Error msg -> Error msg
            | Ok(_, (_,vals')) when vals'.Length <> frame.height ->
                    Error "type error 3"
            | Ok(_, (ctrls', vals')) ->
                let ctrls' = List.tail ctrls'
                Ok(frame, (ctrls', vals'))


    let labeltypes (frame : Frame) =
        match frame.opcode with
        | LOOP -> frame.starttypes
        | _ -> frame.endtypes

    let unreachable (ctrls: Frame list, vals: ValType list) =
        if ctrls.[0].height > vals.Length then
           Ok(ctrls, vals)
        else
            Ok({ctrls.[0] with unreachable = true} :: List.tail ctrls, vals)         


    let End ctrlsvals = 
        match popctrl ctrlsvals with
        | Error msg -> Error msg
        | Ok(frame, cv) ->
            pushvals frame.endtypes cv

    let Else ctrlsvals =  
        match popctrl ctrlsvals with
        | Error msg -> Error msg
        | Ok (frame, _) when frame.opcode <> IF ->
            Error "found a non matching else keyword"
        | Ok(frame, cv) ->
            pushctrl ELSE frame.starttypes frame.endtypes cv


    let rec blockValidater opcode argtypes rettypes codeblock tab mem local ctrlsvals =
        match popvals argtypes ctrlsvals with
        | Error msg -> Error msg
        | Ok(_, cv) ->
            // validate the code of the block
            // This is not optimized as it will run the hole sequence 
            Seq.fold (fun cv instr ->
                match cv with
                | Error msg -> Error msg
                | Ok cv -> validate instr tab mem local cv
            ) (Ok cv) codeblock
            |> fun cv ->
                match cv with
                // error inside the block itself
                | Error msg -> Error msg
                | Ok cv -> pushctrl opcode [] rettypes cv


    and validate opcode tab mem local ctrlsvals =
        match opcode with
        | Nop -> Ok ctrlsvals // correct 

        // this should always be true sinze all constants are
        // typed
        | Const (nt, _) ->
            let tp = typeof (NumType nt)
            pushval (Type tp) ctrlsvals

        | UOP (tp, op) ->
            let tp' = typeof (NumType tp) 
            match uoptypeof op tp' with
            | Error msg -> Error msg
            | Ok() ->
                ctrlsvals
                |> popval (Type tp')
                |> fun ret ->
                    match ret with
                    | Error msg -> Error msg
                    | Ok (_, ret) -> pushval (Type tp') ret
        
        | TOP (tp, _) ->
            let tp' = typeof (NumType tp)
            if (IsInt (Type tp')) <> Ok() then
                Error "not and integer"
            else
                match popval (Type tp') ctrlsvals with
                | Error msg -> Error msg
                | Ok(_, cv) -> pushval (Type valtype.I32) cv

        | Bop (tp, op) ->
            let tp' = typeof (NumType tp)
            match boptypeof op tp' with
            | Error msg -> Error msg
            | Ok () -> // place holder for a true case
                ctrlsvals
                |> popval (Type tp')
                |> fun ret ->
                    match ret with
                    | Error msg -> Error msg
                    | Ok(_, ret) ->
                        match popval (Type tp') ret with
                        | Error msg -> Error msg
                        | Ok(_, ret) -> pushval (Type tp') ret
        
        | ROP (tp, op) ->
            let tp' = typeof (NumType tp)
            match roptypeof op tp' with
            | Error msg -> Error msg
            | Ok() ->
                ctrlsvals
                |> popval (Type tp')
                |> fun ret ->
                    match ret with
                    | Error msg -> Error msg
                    | Ok(_, ret) ->
                        match popval (Type tp') ret with
                        | Error msg -> Error msg
                        | Ok(_, ret) -> pushval (Type valtype.I32) ret
        
        | Drop ->
            ctrlsvals
            |> popval0
            |> Result.map snd
        
        | Select None ->
            match popval (Type (valtype.I32)) ctrlsvals with
            | Error msg -> Error msg
            | Ok(_, cv) ->
                match popval0 cv with
                | Error msg -> Error msg
                | Ok(t1,cv) ->
                    match popval0 cv with
                    | Error msg -> Error msg
                    | Ok(t2, _) when isNum t1 <> Ok() && isNum t2 <> Ok() -> 
                        Error "type error 4"
                    | Ok(t2, _) when t1 <> t2 && t1 <> Unknown && t2 <> Unknown -> 
                        Error "type error 5"
                    | Ok(t2, cv) ->
                        let t = if t1 = Unknown then t2 else t1
                        pushval t cv
          
        | Select (Some t) ->
            let t' = typeof t
            match popval (Type (valtype.I32)) ctrlsvals with
            | Error msg -> Error msg
            | Ok(_,cv) ->
                match popval0 cv with
                | Error msg -> Error msg
                | Ok(_, cv) ->
                    match popval0 cv with
                    | Error msg -> Error msg
                    | Ok(_,cv) -> pushval (Type t') cv

        | UnReachable ->
            unreachable ctrlsvals

        // all block should be considered recursive
        // the blocktype will at some point be made to a sequences of types
        | Block (blocktype, codeblock) ->
            match blocktype with
            | VBlock t ->
                let rettype = [Type (typeof t)]
                blockValidater BLOCK [] rettype codeblock tab mem local ctrlsvals

            | NoType -> blockValidater BLOCK [] [] codeblock tab mem local ctrlsvals
            
            | TBlock idx  ->
                match Map.tryFind idx tab with
                | None -> Error "The function does not exist"
                | Some (FuncType.Func(argtypes, rettypes)) ->
                    let argtypes = List.map (fun t -> Type (typeof t)) argtypes
                    let rettypes = List.map (fun t -> Type (typeof t)) rettypes
                    blockValidater BLOCK argtypes rettypes codeblock tab mem local ctrlsvals
            // this evaluates after all cases
            |> fun cv ->
                match cv with
                | Error msg -> Error msg
                | Ok cv -> End cv

        | Loop (blocktype, codeblock) ->
            match blocktype with
            | VBlock t ->
                let rettype = [Type (typeof t)]
                blockValidater LOOP [] rettype codeblock tab mem local ctrlsvals

            | NoType -> blockValidater LOOP [] [] codeblock tab mem local ctrlsvals
            
            | TBlock idx  ->
                match Map.tryFind idx tab with
                | None -> Error "The function does not exist"
                | Some (FuncType.Func(argtypes, rettypes)) ->
                    let argtypes = List.map (fun t -> Type (typeof t)) argtypes
                    let rettypes = List.map (fun t -> Type (typeof t)) rettypes
                    blockValidater LOOP argtypes rettypes codeblock tab mem local ctrlsvals
            // this evaluates after all cases
            |> fun cv ->
                match cv with
                | Error msg -> Error msg
                | Ok cv -> End cv


        | If (blocktype, iftruecode, iffalsecode) ->
            match popval (Type valtype.I32) ctrlsvals with
            | Error msg -> Error msg
            | Ok(_, cv) ->
                match blocktype with
                | VBlock t ->
                    let rettype = [Type (typeof t)]
                    blockValidater IF [] rettype iftruecode tab mem local cv
                    |> fun cv ->
                        match cv with
                        | Error msg -> Error msg
                        | Ok cv -> Else cv
                        |> Result.map (fun cv -> [], cv)

                | NoType -> 
                    blockValidater IF [] [] iftruecode tab mem local cv
                    |> fun cv ->
                        match cv with
                        | Error msg -> Error msg
                        | Ok cv -> Else cv
                        |> Result.map (fun cv -> [], cv)

                | TBlock idx  ->
                    match Map.tryFind idx tab with
                    | None -> Error "The function does not exist"
                    | Some (FuncType.Func(argtypes, rettypes)) ->
                        let argtypes = List.map (fun t -> Type (typeof t)) argtypes
                        let rettypes = List.map (fun t -> Type (typeof t)) rettypes
                        blockValidater IF argtypes rettypes iftruecode tab mem local cv
                        |> fun cv ->
                            match cv with
                            | Error msg -> Error msg
                            | Ok cv -> 
                                Else cv
                                |> Result.map (fun cv -> argtypes, cv)
                // evaluated true branch 
                |> fun afterward ->
                    match afterward with
                    | Error msg -> Error msg
                    // validating false branch
                    | Ok (argtypes, cv) ->
                        match popvals argtypes cv with
                        | Error msg -> Error msg
                        | Ok(_, cv) ->
                            // validate the code of the block
                            // This is not optimized as it will run the hole sequence 
                            Seq.fold (fun cv instr ->
                                match cv with
                                | Error msg -> Error msg
                                | Ok cv -> validate instr tab mem local cv
                            ) (Ok cv)   iffalsecode
                            |> fun cv ->
                                match cv with
                                // error inside the block itself
                                | Error msg -> Error msg
                                | Ok cv -> End cv
        
        // ------------------------------------------- //
        | Br n ->
            if tablesize < n then
                Error "address out of range"
            else
                let frame = (fst ctrlsvals).[int n]
                popvals (labeltypes frame) ctrlsvals
                |> Result.map snd
            
        | Br_if n ->
            match popval (Type valtype.I32) ctrlsvals with
            | Error msg -> Error msg
            | Ok(_, cv) ->
                if tablesize < n then
                    Error "address out of range"
                else
                    let frame = (fst cv).[int n]
                    let lt = labeltypes frame
                    match popvals lt cv with
                    | Error msg -> Error msg
                    | Ok(_,cv) -> pushvals lt cv
                    

            
        | Br_table (lidx, lidx') ->
            let m = int lidx'
            match popval (Type valtype.I32) ctrlsvals with
            | Error msg -> Error msg
            | Ok(_, (c,v)) ->
                if tablesize < lidx' then
                    Error "address out of range"
                else
                    let arity = (labeltypes c.[m]).Length
                    Seq.fold (fun acc n ->
                        match acc with
                        | Error msg -> Error msg
                        | Ok(c : Frame list,v) ->
                            if tablesize < n then
                                Error "address out of range"
                            elif (labeltypes c.[int n]).Length <> arity then
                                Error "addres out of range 2"
                            else
                                match popvals (labeltypes c.[int n]) (c,v) with
                                | Error msg -> Error msg
                                | Ok(popped,(c,v)) ->
                                    pushvals popped (c,v)
                    ) (Ok(c,v)) lidx
                    |> fun cv ->
                        match cv with
                        | Error msg -> Error msg
                        | Ok cv ->
                            popvals (labeltypes c.[m]) cv
                            |> Result.map snd
                            
        | Call _ ->
            sprintf "%A not implemented" opcode
            |> Error
            
        | Call_Indirect _ ->
            sprintf "%A not implemented" opcode
            |> Error

        | Convert _ ->
            sprintf "%A not implemented" opcode
            |> Error

        | DataDrop _ ->
            sprintf "%A not implemented" opcode
            |> Error
        
        | Demote ->
            sprintf "%A not implemented" opcode
            |> Error

        | Promote ->
            sprintf "%A not implemented" opcode
            |> Error
        
        | ElemDrop _ -> 
            sprintf "%A not implemented" opcode
            |> Error
            
        | Extend _ ->
            sprintf "%A not implemented" opcode
            |> Error
        
        | Extend_s _ ->
            sprintf "%A not implemented" opcode
            |> Error

        | RefNull _ ->
            sprintf "%A not implemented" opcode
            |> Error
                
        | RefIsNul ->
            sprintf "%A not implemented" opcode
            |> Error

        | RefFunc _ ->
            sprintf "%A not implemented" opcode
            |> Error
        
        // variable instructions
        | LocalGet _ ->
            sprintf "%A not implemented" opcode
            |> Error 
        | LocalSet _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | LocalTee _ ->
            sprintf "%A not implemented" opcode
            |> Error // like local.set but leave the value on the top of the stack
        | GlobalGet _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | GLobalSet _ ->
            sprintf "%A not implemented" opcode
            |> Error
           

        // Table instructions
        | TableGet _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableSet _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableSize _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableGrow _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableFill _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableCopy _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | TableInit _ ->
            sprintf "%A not implemented" opcode
            |> Error
        

           
        // Memory Instructions
        | Load _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | Store _ ->
            sprintf "%A not implemented" opcode
            |> Error
        | MemSize ->
            sprintf "%A not implemented" opcode
            |> Error
        | MemGrow ->
            sprintf "%A not implemented" opcode
            |> Error
        | MemFill ->
            sprintf "%A not implemented" opcode
            |> Error
        | MemCopy ->
            sprintf "%A not implemented" opcode
            |> Error
        | MemInint _ ->
            sprintf "%A not implemented" opcode
            |> Error 

        | Return ->
            sprintf "%A not implemented" opcode
            |> Error

        | I32Wrap64 ->
            sprintf "%A not implemented" opcode
            |> Error

        | ReInterpret _ ->
            sprintf "%A not implemented" opcode
            |> Error
            
        | Trunc _ ->
            sprintf "%A not implemented" opcode
            |> Error

        | Trunc_Sat _ ->
            sprintf "%A not implemented" opcode
            |> Error

