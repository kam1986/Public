namespace Binary
(*

    Decoding of Vec128 instructions are not yet implemented

*)
module Decode =
    
    open System
    open Types
    open Values
    open Wasm
    
    type Result<'a,'b> with
        static member mapWithError f ret =
            match ret with
            | Error msg -> Error msg
            | Ok ret -> f ret


    let [<Literal>] ExtR  = 0x60uy
    let [<Literal>] FuncR = 0x70uy
    let [<Literal>] INT32 = 0x7Fuy
    let [<Literal>] INT64 = 0x7Euy
    let [<Literal>] FLT32 = 0x7Duy
    let [<Literal>] FLT64 = 0x7Cuy

    let (!) = Seq.tail 
    let (~%) = Seq.tryHead 

    let ceil n = int (ceil (float n/7.))

    type 'a EndCases =
        | Normal of 'a
        | End    of 'a
        | Else   of 'a
    with
        static member map f case =
            match case with
            | Normal c -> Normal (f c)
            | End    c -> End (f c)
            | Else   c -> Else (f c)

        static member Value case =
            match case with
            | Normal c -> c
            | End    c -> c
            | Else   c -> c
            

    let eof = Error "end of file"

    let End a  = Ok(End a) 
    let Else a = Ok(Else a)
    let Normal a = Ok(Normal a)
   
   

    let rec DecodeInt N bytes = 
        match %bytes with
        | None -> eof
        | Some n when n < 128uy && uint n < (1u <<< N-1) -> Ok(int64 n, !bytes)
        | Some n when 68uy <= n && n < 128uy && uint n > 128u - (1u <<< N-2) ->  Ok(int64 n - 128L, !bytes)
        | Some n when uint n >= 128u && N > 7 -> 
            DecodeInt (N-1) !bytes
            |> Result.map(fun (m, bytes) -> 128L * m + (int64 n - 128L), bytes)
        | _ -> Error "Not a legal int value"


    let rec DecodeUInt N bytes =
        match %bytes with
        | None -> eof
        | Some n when n < 64uy  && uint n < (1u <<< N-1) -> Ok(uint64 n, !bytes)
        | Some n when n >= 64uy && N > 7 -> 
            DecodeUInt (N-1) !bytes
            |> Result.map (fun (m, bytes) -> (128UL * m + (uint64 n - 128UL), bytes))
        | _ -> Error "Not a legal uint value"

    let DecodeI32 bytes = 
        (DecodeInt 32 bytes)
        |> Result.map (fun (v,bytes) -> int v, bytes)

    let DecodeU32 bytes = 
        (DecodeInt 32 bytes)
        |> Result.map (fun (v,bytes) -> uint v, bytes)
    
    let DecodeI64 bytes = DecodeInt 64 bytes


    let DecodeU64 bytes = DecodeInt 64 bytes


    let DecodeF32 (bytes: byte seq) =
        try
            Seq.take 4 bytes
            |> Seq.toArray
            |> fun b -> Ok(BitConverter.ToSingle(b, 0), Seq.skip 4 bytes)
        with
            _ -> eof
        

    let DecodeF64 (bytes: byte seq) =
        try
            Seq.take 8 bytes
            |> Seq.toArray
            |> fun b -> Ok(BitConverter.ToDouble(b, 0), Seq.skip 8 bytes)
        with
            _ -> eof

    
    let DecodeVecOf f bytes =
        let rec loop n acc bytes =
            if n = 0u then 
                Ok(List.rev acc, bytes)
            else
                match f bytes with
                | Error msg -> Error msg
                | Ok(ret, bytes) -> loop (n-1u) (ret :: acc) bytes

        match DecodeU32 bytes with
        | Error msg -> Error msg
        | Ok(sz, bytes) -> loop sz [] bytes


    let DecodeUTF8 bytes =    
        match %bytes with
        | None -> eof
        | Some b0 when b0 < 0b10000000uy -> Ok([|b0|], bytes)
        | Some b0 when b0 < 0b1100000uy -> 
            let bytes = !bytes
            match %bytes with
            | None -> eof
            | Some b1 when  0b10000000uy < b1 && b1 < 0b11000000uy ->
                Ok([|b0;b1|], bytes)
            | Some b -> Error "UTF8 Error: wron format of trailing byte"

        | Some b0 when b0 < 0b11100000uy ->
            let bytes = !bytes
            match %bytes with
            | None -> eof
            | Some b1 when 0b10000000uy < b1 && b1 < 0b11000000uy -> 
                let bytes = !bytes
                match %bytes with
                | None -> eof
                | Some b2 when 0b10000000uy < b2 && b2 < 0b11000000uy -> Ok([|b0; b1; b2|], bytes)
                | Some b -> Error "UTF8 Error: wron format of trailing byte"
                
            | Some b -> Error "UTF8 Error: wron format of trailing byte"
                
        | Some b0 when b0 < 0b11110000uy -> 
            let bytes = !bytes
            match %bytes with
            | None -> eof
            | Some b1 when 0b10000000uy < b1 && b1 < 0b11000000uy -> 
                let bytes = !bytes
                match %bytes with
                | None -> eof
                | Some b2 when 0b10000000uy < b2 && b2 < 0b11000000uy -> 
                    let bytes = !bytes
                    match %bytes with
                    | None -> eof
                    | Some b3 when 0b10000000uy < b3 && b3 < 0b11000000uy -> Ok([|b0; b1; b2; b3|], bytes)
                    | Some b -> Error "UTF8 Error: wron format of trailing byte"
                        
                | Some b -> Error "UTF8 Error: wron format of trailing byte"
                
            | Some b -> Error "UTF8 Error: wron format of trailing byte"
            
        | Some b -> Error $"UFT8 Error: {b} is not a valid header byte"

    let DecodeName bytes =
        DecodeVecOf DecodeUTF8 bytes
        |> Result.map (fun (chars, bytes) ->
                System.Text.Encoding.UTF8.GetString(Array.concat chars), bytes
            )

    let DecodeNumType bytes : Result<numType * byte seq, string> =
        match %bytes with
        | None -> eof
        | Some 0x7Fuy -> Ok(I32(), !bytes)
        | Some 0x7Euy -> Ok(I64(), !bytes)
        | Some 0x7Duy -> Ok(F32(), !bytes)
        | Some 0x7Cuy -> Ok(F64(), !bytes)
        | Some opcode -> Error $"{opcode} is not a numtyer"


    let DecodeRefType bytes =
        match %bytes with
        | Some 0x70uy -> Ok(refType.FuncRef, !bytes)
        | Some 0x6Fuy -> Ok(refType.ExternRef, !bytes)
        | Some opcode -> Error $"{opcode} not a reference type"
        | None -> eof

    let DecodeValueType bytes = 
        match DecodeNumType bytes with
        | Ok (numtype, bytes) -> Ok(NumType numtype, bytes)
        | _ -> DecodeRefType bytes |> Result.map (fun (reftype, bytes) -> RefType reftype, bytes)
        
    let DecodeResultType bytes = 
        DecodeVecOf DecodeValueType bytes
        |> Result.map (fun (rettype, bytes) -> Result rettype, bytes)

    let DecodeFunctionType bytes =
        match %bytes with
        | None -> eof 
        | Some 0x60uy ->
            match DecodeResultType !bytes with
            | Error msg -> Error msg
            | Ok(input, bytes) ->
                DecodeResultType bytes
                |> Result.map (fun (output, bytes) -> FuncType(input, output), bytes)
        | _ -> Error "Expected 0x60 header"
            
    let DecodeLimit bytes =
        match %bytes with
        | None -> eof
        | Some 0x00uy -> 
            DecodeU32 !bytes 
            |> Result.map (fun (min, bytes) -> 
                    { min = min; max = None; share = Shared}, bytes
                )

        | Some 0x01uy ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(min, bytes) ->
                DecodeU32 bytes         
                |> Result.map (fun (max, bytes) -> 
                        { min = min; max = Some max; share = Shared }, bytes
                    )

        | Some _ -> Error "Expected 0x00 or 0x01 header"

    let DecodeMemoryType bytes =
        DecodeLimit bytes
        |> Result.map (fun (limit, bytes) -> MemoryType limit, bytes)

    let DecodeTableType bytes =
        match DecodeRefType bytes with
        | Error msg -> Error msg
        | Ok(reftype, bytes) ->
            DecodeLimit bytes
            |> Result.map (fun (limit, bytes) -> TableType(limit, reftype), bytes)

    let DecodeGlobalType bytes =
        match DecodeValueType bytes with
        | Error msg -> Error msg
        | Ok(valtype, bytes) ->
            match %bytes with
            | None -> eof
            | Some 0x00uy -> Ok(GlobalType(valtype, Immutable),!bytes)
            | Some 0x01uy -> Ok(GlobalType(valtype, Mutable),!bytes)
            | _ -> Error "Expected 0x00 or 0x01 footer"


    let DecodeBlockType bytes =
        match %bytes with
        | None        -> eof
        | Some 0x40uy -> 
            Ok (None, !bytes)
             |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)

        | Some ExtR   -> 
            Ok (Some (RefType ExternRef), !bytes) 
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
        
        | Some FuncR  -> 
            Ok (Some (RefType refType.FuncRef), !bytes)   
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
            
        | Some INT32    -> 
            Ok (Some (NumType (I32())), !bytes)    
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
            
        | Some INT64    -> 
            Ok (Some (NumType (I64())), !bytes)     
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
            
        | Some FLT32    -> 
            Ok (Some (NumType (F32())), !bytes)     
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
            
        | Some FLT64    -> 
            Ok (Some (NumType (F64())), !bytes)     
            |> Result.map (fun (t, bytes) -> ValBlockType t, bytes)
            
        | _      -> 
            DecodeInt 33 bytes 
            |> Result.map (fun (i, bytes) -> VarBlockType i, bytes)

    
    let DecodeMemArg bytes =
        match DecodeU32 bytes with
        | Error msg -> Error msg
        | Ok(a, bytes) ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(o, bytes) -> Ok((a, o), bytes)

    let rec DecodeInstruction bytes =
        let rec step acc bytes =
            match %bytes with
            | None          -> Normal(List.rev acc, bytes)
            | Some 0x00uy   -> step (Unreachable() :: acc) !bytes
            | Some 0x01uy   -> step (Nop() :: acc) !bytes
            | Some 0x02uy   -> 
                match DecodeBlockType !bytes with
                | Ok(bt, bytes) ->
                    // find nested code
                    match step [] bytes with
                    | Ok(End(body, bytes)) -> 
                        let b = Block(bt, body,())
                        step (b :: acc) bytes
                    | Ok _ -> Error "Wrong block format"
                    | err -> err
                | Error msg -> Error msg

            | Some 0x03uy   ->
                match DecodeBlockType !bytes with
                | Error msg -> Error msg
                | Ok(bt, bytes) ->
                    // find nested code
                    match step [] bytes with
                    | Error msg -> Error msg
                    | Ok(End(body, bytes)) -> 
                        let l = Loop(bt, body,())
                        step (l :: acc) bytes
                    | Ok _ -> Error "Wrong block format"
                    

            | Some 0x04uy   ->
                match DecodeBlockType !bytes with
                | Error msg -> Error msg
                | Ok(bt, bytes) ->
                    // find nested code
                    match step [] bytes with
                    | Error msg -> Error msg
                    | Ok(End(tbody, bytes)) -> 
                        let i = If(bt, tbody, [], ())
                        step (i :: acc) bytes

                    | Ok(Else(tbody, bytes)) ->
                        match step [] bytes with
                        | Ok(End(fbody, bytes)) ->
                            let i = If(bt, tbody, fbody, ())
                            step (i :: acc) bytes
                        | Ok(Else _)     -> Error "Found else tag instead of and end tag for the if instruction"
                        | Ok(Normal _)   -> Error "Found no end tag for the if instruction"
                        | err -> err
                   
                    | Ok _ -> Error "Found no end or else tag for the if instruction"

            | Some 0x05uy   -> Else(List.rev acc, !bytes)
            // | Some 0x06uy   -> 
            // | Some 0x07uy   -> 
            // | Some 0x08uy   ->  not defined ops
            // | Some 0x09uy   -> 
            // | Some 0x0Auy   -> 
            | Some 0x0Buy   -> End(List.rev acc, !bytes) // to be catch by the decoder for nested instructions
            | Some 0x0Cuy   -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(labelidx, bytes) -> step (Br(labelidx,()) :: acc) bytes

            | Some 0x0Duy   ->
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(labelidx, bytes) -> step (BrIf(labelidx,()) :: acc) bytes

            | Some 0x0Euy   -> 
                match DecodeVecOf DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(labels, bytes) ->
                    match DecodeU32 bytes with
                    | Error msg -> Error msg
                    | Ok(labelidx, bytes) -> step (BrTable(labels, labelidx, ()) :: acc) bytes

            | Some 0x0Fuy   -> step (Return() :: acc) !bytes
            | Some 0x10uy   -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(fidx, bytes) -> step (Call(fidx,()) :: acc) bytes

            | Some 0x11uy   -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(typeidx, bytes) -> 
                    match DecodeU32 bytes with
                    | Error msg -> Error msg
                    | Ok(tableidx, bytes) -> step (CallIndirect(typeidx, tableidx,()) :: acc) bytes

            | Some 0xD0uy ->
                match DecodeRefType !bytes with
                | Error msg -> Error msg
                | Ok(reftype, bytes) -> step (RefNull(reftype,()) :: acc) bytes 

            | Some 0xD1uy -> step (RefIsNull() :: acc) bytes 
            | Some 0xD2uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(fidx, bytes) -> step (RefFunc(fidx,()) :: acc) bytes

            | Some 0x1Auy -> step (Drop(Stack,()) :: acc) bytes 
            | Some 0x1Buy -> step (Select([],()) :: acc) bytes 
            | Some 0x1Cuy -> 
                match DecodeVecOf DecodeValueType !bytes with
                | Error msg -> Error msg
                | Ok(valtypes, bytes) ->
                    step (Select(valtypes, ()) :: acc) bytes
            

            // variable and table instructions 
            | Some 0x20uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Load(Local idx,()) :: acc) bytes
            
            | Some 0x21uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Store(Local idx,()) :: acc) bytes

            | Some 0x22uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Tee(Local idx,()) :: acc) bytes

            | Some 0x23uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Load(Global idx,()) :: acc) bytes

            | Some 0x24uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Store(Global idx,()) :: acc) bytes

            | Some 0x25uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Load(Table idx,()) :: acc) bytes

            | Some 0x26uy -> 
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(idx, bytes) -> step (Store(Table idx,()) :: acc) bytes

            | Some 0xFCuy ->
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(op, bytes) ->
                    match op with
                    | 0u -> step (Spec(I32 IntOp.TruncSatF32, ()) :: acc) bytes 
                    | 1u -> step (Spec(U32 IntOp.TruncSatF32, ()) :: acc) bytes
                    | 2u -> step (Spec(I32 IntOp.TruncSatF64, ()) :: acc) bytes
                    | 3u -> step (Spec(U32 IntOp.TruncSatF64, ()) :: acc) bytes
                    | 4u -> step (Spec(I64 IntOp.TruncSatF32, ()) :: acc) bytes
                    | 5u -> step (Spec(U64 IntOp.TruncSatF32, ()) :: acc) bytes
                    | 6u -> step (Spec(I64 IntOp.TruncSatF64, ()) :: acc) bytes
                    | 7u -> step (Spec(U64 IntOp.TruncSatF64, ()) :: acc) bytes
                    | 8u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(dataidx, bytes) ->
                           match DecodeU32 bytes with
                           | Error msg -> Error msg
                           | Ok(0u as dataidx, bytes) -> step (Init(Memory { ty = I32(); name = dataidx; align=0; offset=0; sz = None},Memory { ty = I32(); name = 0u; align=0; offset=0; sz = None} ,()) :: acc) !bytes
                           | Ok(n, _) -> Error $"expected 0x00 but got {n} as argument of memory.init"
                    
                    | 9u ->
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(0u as dataidx, bytes) ->
                            let mem = { ty = I32(); name = dataidx; align=0; offset=0; sz = None}
                            step (Drop(Memory mem,()) :: acc) !bytes
                        | Ok(n,_) -> Error $"expected 0x00 but got {n} as argument of memory.copy"
                    
                    | 10u ->
                        match DecodeU32 !bytes with 
                        | Error msg -> Error msg
                        | Ok(0u as n, bytes) ->
                            match DecodeU32 bytes with 
                            | Error msg -> Error msg
                            | Ok(0u as m, bytes) ->
                                let mem1 = { ty = I32(); name = n; align=0; offset=0; sz = None}
                                let mem2 = { ty = I32(); name = m; align=0; offset=0; sz = None}
                                step (Copy(Memory mem1, Memory mem2, ()) :: acc) bytes

                            | Ok(n,_) -> Error $"expected 0x00 but got {n} as second argument of memory.copy"
                        | Ok(n,_) -> Error $"expected 0x00 but got {n} as first argument of memory.copy"
                        
                    | 11u ->
                        match DecodeU32 !bytes with 
                        | Error msg -> Error msg
                        | Ok(0u as n, bytes) -> 
                            let mem = { ty = I32(); name = n; align=0; offset=0; sz = None}
                            step (Fill(Memory mem, ()) :: acc) bytes

                        | Ok(n,_) -> Error $"expected 0x00 but got {n} as argument of memory.fill"
                    | 12u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(elmidx, bytes) ->
                            match DecodeU32 bytes with
                            | Error msg -> Error msg
                            | Ok(tabidx, bytes) -> step (Init(Table elmidx, Table tabidx,())  :: acc) bytes
                    
                    | 13u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(elmidx, bytes) -> step (Drop(Table elmidx,())  :: acc) bytes
                    
                    | 14u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(tabidx1, bytes) ->
                            match DecodeU32 bytes with
                            | Error msg -> Error msg
                            | Ok(tabidx2, bytes) -> step (Copy(Table tabidx1, Table tabidx2, ())  :: acc) bytes
                    
                    | 15u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(elmidx, bytes) -> step (Grow(Table elmidx,())  :: acc) bytes
                    
                    | 16u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(elmidx, bytes) -> step (Size(Table elmidx,())  :: acc) bytes
                    
                    | 17u -> 
                        match DecodeU32 bytes with
                        | Error msg -> Error msg
                        | Ok(elmidx, bytes) -> step (Fill(Table elmidx,())  :: acc) bytes

                    | _   -> Error $"{op} not a legal encoding of any table instructions"

            | Some 0x28uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x29uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Auy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = F32(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Buy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = F64(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Cuy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, SX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Duy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, ZX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Euy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, SX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x2Fuy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, ZX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x30uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, SX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x31uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, ZX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x32uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, SX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x33uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, ZX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x34uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack32, SX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x35uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack32, ZX)}
                    step (Load(Memory mem,()) :: acc) bytes

            | Some 0x36uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x37uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x38uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = F32(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x39uy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = F64(); name = 0u; align = int align; offset = int offset; sz = None}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x3Auy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty =I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, SX)}
                    step (Store(Memory mem,()) :: acc) bytes
            
            | Some 0x3Buy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I32(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, SX)}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x3Cuy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack8, SX)}
                    step (Store(Memory mem,()) :: acc) bytes


            | Some 0x3Duy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack16, SX)}
                    step (Store(Memory mem,()) :: acc) bytes

            | Some 0x3Euy ->
                match DecodeMemArg !bytes with
                | Error msg -> Error msg
                | Ok((align, offset), bytes) ->
                    let mem = { ty = I64(); name = 0u; align = int align; offset = int offset; sz = Some(Pack32, SX)}
                    step (Store(Memory mem,()) :: acc) bytes


            | Some 0x3Fuy ->
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(0u as n, bytes) ->
                    let mem = { ty = I32(); name = n; align = 0; offset = 0; sz = Some(Pack32, SX)}
                    step (Size(Memory mem,()) :: acc) bytes
                | Ok(n, _) -> Error $"Expected 0x00 but got {n} as argument for memory.size"
                    
            | Some 0x40uy ->
                match DecodeU32 !bytes with
                | Error msg -> Error msg
                | Ok(0u as n, bytes) ->
                    let mem = { ty = I32(); name = n; align = 0; offset = 0; sz = Some(Pack32, SX)}
                    step (Grow(Memory mem,()) :: acc) bytes
                | Ok(n, _) -> Error $"Expected 0x00 but got {n} as argument for memory.size"

            | Some 0x41uy ->
                match DecodeI32 !bytes with
                | Error msg -> Error msg
                | Ok(value, bytes) -> step (Const(I32 value, ()) :: acc) bytes

            | Some 0x42uy ->
                match DecodeI64 !bytes with
                | Error msg -> Error msg
                | Ok(value, bytes) -> step (Const(I64 value, ()) :: acc) bytes

            | Some 0x43uy ->
                match DecodeF32 !bytes with
                | Error msg -> Error msg
                | Ok(value, bytes) -> step (Const(F32 value, ()) :: acc) bytes
            
            | Some 0x44uy ->
                match DecodeF64 !bytes with
                | Error msg -> Error msg
                | Ok(value, bytes) -> step (Const(F64 value, ()) :: acc) bytes

            | Some 0x45uy -> step (Test(I32 IntOp.Eqz, ()) :: acc)     !bytes
            | Some 0x46uy -> step (Compare(I32 IntOp.Eq, ()) :: acc)   !bytes
            | Some 0x47uy -> step (Compare(I32 IntOp.Ne, ()) :: acc)   !bytes
            | Some 0x48uy -> step (Compare(I32 IntOp.Lt, ()) :: acc)   !bytes
            | Some 0x49uy -> step (Compare(U32 IntOp.Lt, ()) :: acc)   !bytes
            | Some 0x4Auy -> step (Compare(I32 IntOp.Gt, ()) :: acc)   !bytes
            | Some 0x4Buy -> step (Compare(U32 IntOp.Gt, ()) :: acc)   !bytes
            | Some 0x4Cuy -> step (Compare(I32 IntOp.Le, ()) :: acc)   !bytes
            | Some 0x4Duy -> step (Compare(U32 IntOp.Le, ()) :: acc)   !bytes
            | Some 0x4Euy -> step (Compare(I32 IntOp.Ge, ()) :: acc)   !bytes
            | Some 0x4Fuy -> step (Compare(U32 IntOp.Ge, ()) :: acc)   !bytes

            | Some 0x51uy -> step (Test(I64 IntOp.Eqz, ()) :: acc)     !bytes
            | Some 0x50uy -> step (Compare(I64 IntOp.Eq, ()) :: acc)   !bytes
            | Some 0x52uy -> step (Compare(I64 IntOp.Ne, ()) :: acc)   !bytes
            | Some 0x53uy -> step (Compare(I64 IntOp.Lt, ()) :: acc)   !bytes
            | Some 0x54uy -> step (Compare(I64 IntOp.Lt, ()) :: acc)   !bytes
            | Some 0x55uy -> step (Compare(I64 IntOp.Gt, ()) :: acc)   !bytes
            | Some 0x56uy -> step (Compare(I64 IntOp.Gt, ()) :: acc)   !bytes
            | Some 0x57uy -> step (Compare(I64 IntOp.Le, ()) :: acc)   !bytes
            | Some 0x58uy -> step (Compare(I64 IntOp.Le, ()) :: acc)   !bytes
            | Some 0x59uy -> step (Compare(I64 IntOp.Ge, ()) :: acc)   !bytes
            | Some 0x5Auy -> step (Compare(I64 IntOp.Ge, ()) :: acc)   !bytes

            | Some 0x5Buy -> step (Compare(F32 FloatOp.Eq, ()) :: acc) !bytes
            | Some 0x5Cuy -> step (Compare(F32 FloatOp.Ne, ()) :: acc) !bytes
            | Some 0x5Duy -> step (Compare(F32 FloatOp.Lt, ()) :: acc) !bytes
            | Some 0x5Euy -> step (Compare(F32 FloatOp.Gt, ()) :: acc) !bytes
            | Some 0x5Fuy -> step (Compare(F32 FloatOp.Le, ()) :: acc) !bytes
            | Some 0x60uy -> step (Compare(F32 FloatOp.Ge, ()) :: acc) !bytes

            | Some 0x61uy -> step (Compare(F64 FloatOp.Eq, ()) :: acc) !bytes
            | Some 0x62uy -> step (Compare(F64 FloatOp.Ne, ()) :: acc) !bytes
            | Some 0x63uy -> step (Compare(F64 FloatOp.Lt, ()) :: acc) !bytes
            | Some 0x64uy -> step (Compare(F64 FloatOp.Gt, ()) :: acc) !bytes
            | Some 0x65uy -> step (Compare(F64 FloatOp.Le, ()) :: acc) !bytes
            | Some 0x66uy -> step (Compare(F64 FloatOp.Ge, ()) :: acc) !bytes

            | Some 0x67uy -> step (Unary(I32 IntOp.Clz, ()) :: acc)    !bytes
            | Some 0x68uy -> step (Unary(I32 IntOp.Ctz, ()) :: acc)    !bytes
            | Some 0x69uy -> step (Unary(I32 IntOp.Popcnt, ()) :: acc) !bytes
            | Some 0x6Auy -> step (Binary(I32 IntOp.Add, ()) :: acc)   !bytes
            | Some 0x6Buy -> step (Binary(I32 IntOp.Sub, ()) :: acc)   !bytes
            | Some 0x6Cuy -> step (Binary(I32 IntOp.Mul, ()) :: acc)   !bytes
            | Some 0x6Duy -> step (Binary(I32 IntOp.Div, ()) :: acc)   !bytes
            | Some 0x6Euy -> step (Binary(U32 IntOp.Div, ()) :: acc)   !bytes
            | Some 0x6Fuy -> step (Binary(I32 IntOp.Rem, ()) :: acc)   !bytes
            | Some 0x70uy -> step (Binary(U32 IntOp.Rem, ()) :: acc)   !bytes
            | Some 0x71uy -> step (Binary(I32 IntOp.And, ()) :: acc)   !bytes
            | Some 0x72uy -> step (Binary(I32 IntOp.Or, ()) :: acc)    !bytes
            | Some 0x73uy -> step (Binary(I32 IntOp.Xor, ()) :: acc)   !bytes
            | Some 0x74uy -> step (Binary(I32 IntOp.Shl, ()) :: acc)   !bytes
            | Some 0x75uy -> step (Binary(I32 IntOp.Shr, ()) :: acc)   !bytes
            | Some 0x76uy -> step (Binary(U32 IntOp.Shr, ()) :: acc)   !bytes
            | Some 0x77uy -> step (Binary(I32 IntOp.Rotl, ()) :: acc)  !bytes
            | Some 0x78uy -> step (Binary(I32 IntOp.Rotr, ()) :: acc)  !bytes

            | Some 0x79uy -> step (Unary(I64 IntOp.Clz, ()) :: acc)    !bytes
            | Some 0x7Auy -> step (Unary(I64 IntOp.Ctz, ()) :: acc)    !bytes
            | Some 0x7Buy -> step (Unary(I64 IntOp.Popcnt, ()) :: acc) !bytes
            | Some 0x7Cuy -> step (Binary(I64 IntOp.Add, ()) :: acc)   !bytes
            | Some 0x7Duy -> step (Binary(I64 IntOp.Sub, ()) :: acc)   !bytes
            | Some 0x7Euy -> step (Binary(I64 IntOp.Mul, ()) :: acc)   !bytes
            | Some 0x7Fuy -> step (Binary(I64 IntOp.Div, ()) :: acc)   !bytes
            | Some 0x80uy -> step (Binary(U64 IntOp.Div, ()) :: acc)   !bytes
            | Some 0x81uy -> step (Binary(I64 IntOp.Rem, ()) :: acc)   !bytes
            | Some 0x82uy -> step (Binary(U64 IntOp.Rem, ()) :: acc)   !bytes
            | Some 0x83uy -> step (Binary(I64 IntOp.And, ()) :: acc)   !bytes
            | Some 0x84uy -> step (Binary(I64 IntOp.Or, ()) :: acc)    !bytes
            | Some 0x85uy -> step (Binary(I64 IntOp.Xor, ()) :: acc)   !bytes
            | Some 0x86uy -> step (Binary(I64 IntOp.Shl, ()) :: acc)   !bytes
            | Some 0x87uy -> step (Binary(I64 IntOp.Shr, ()) :: acc)   !bytes
            | Some 0x88uy -> step (Binary(U64 IntOp.Shr, ()) :: acc)   !bytes
            | Some 0x89uy -> step (Binary(I64 IntOp.Rotl, ()) :: acc)  !bytes
            | Some 0x8Auy -> step (Binary(I64 IntOp.Rotr, ()) :: acc)  !bytes

            | Some 0x8Buy -> step (Unary(F32 FloatOp.Abs, ()) :: acc) !bytes
            | Some 0x8Cuy -> step (Unary(F32 FloatOp.Neg, ()) :: acc) !bytes
            | Some 0x8Duy -> step (Unary(F32 FloatOp.Ceil, ()) :: acc) !bytes
            | Some 0x8Euy -> step (Unary(F32 FloatOp.Floor, ()) :: acc) !bytes
            | Some 0x8Fuy -> step (Unary(F32 FloatOp.Trunc, ()) :: acc) !bytes
            | Some 0x90uy -> step (Unary(F32 FloatOp.Nearest, ()) :: acc) !bytes
            | Some 0x91uy -> step (Unary(F32 FloatOp.Sqrt, ()) :: acc) !bytes
            | Some 0x92uy -> step (Binary(F32 FloatOp.Add, ()) :: acc) !bytes
            | Some 0x93uy -> step (Binary(F32 FloatOp.Sub, ()) :: acc) !bytes
            | Some 0x94uy -> step (Binary(F32 FloatOp.Mul, ()) :: acc) !bytes
            | Some 0x95uy -> step (Binary(F32 FloatOp.Div, ()) :: acc) !bytes
            | Some 0x96uy -> step (Binary(F32 FloatOp.Min, ()) :: acc) !bytes
            | Some 0x97uy -> step (Binary(F32 FloatOp.Max, ()) :: acc) !bytes
            | Some 0x98uy -> step (Binary(F32 FloatOp.CopySign, ()) :: acc) !bytes
            | Some 0x99uy -> step (Unary(F64 FloatOp.Abs, ()) :: acc) !bytes
            | Some 0x9Auy -> step (Unary(F64 FloatOp.Neg, ()) :: acc) !bytes
            | Some 0x9Buy -> step (Unary(F64 FloatOp.Ceil, ()) :: acc) !bytes
            | Some 0x9Cuy -> step (Unary(F64 FloatOp.Floor, ()) :: acc) !bytes
            | Some 0x9Duy -> step (Unary(F64 FloatOp.Trunc, ()) :: acc) !bytes
            | Some 0x9Euy -> step (Unary(F64 FloatOp.Nearest, ()) :: acc) !bytes
            | Some 0x9Fuy -> step (Unary(F64 FloatOp.Sqrt, ()) :: acc) !bytes
            | Some 0xA0uy -> step (Binary(F64 FloatOp.Add, ()) :: acc) !bytes
            | Some 0xA1uy -> step (Binary(F64 FloatOp.Sub, ()) :: acc) !bytes
            | Some 0xA2uy -> step (Binary(F64 FloatOp.Mul, ()) :: acc) !bytes
            | Some 0xA3uy -> step (Binary(F64 FloatOp.Div, ()) :: acc) !bytes
            | Some 0xA4uy -> step (Binary(F64 FloatOp.Min, ()) :: acc) !bytes
            | Some 0xA5uy -> step (Binary(F64 FloatOp.Max, ()) :: acc) !bytes
            | Some 0xA6uy -> step (Binary(F64 FloatOp.CopySign, ()) :: acc) !bytes
            
            | Some 0xA7uy -> step (Convert(I32 IntOp.WrapI64,()) :: acc) !bytes 
            | Some 0xA8uy -> step (Convert(I32 IntOp.TruncF32,()) :: acc) !bytes
            | Some 0xA9uy -> step (Convert(U32 IntOp.TruncF32,()) :: acc) !bytes
            | Some 0xAAuy -> step (Convert(I32 IntOp.TruncF64,()) :: acc) !bytes
            | Some 0xABuy -> step (Convert(U32 IntOp.TruncF64,()) :: acc) !bytes
            | Some 0xACuy -> step (Convert(I64 IntOp.Extend,()) :: acc) !bytes
            | Some 0xADuy -> step (Convert(U64 IntOp.Extend,()) :: acc) !bytes
            | Some 0xAEuy -> step (Convert(I64 IntOp.TruncF32,()) :: acc) !bytes
            | Some 0xAFuy -> step (Convert(U64 IntOp.TruncF32,()) :: acc) !bytes
            | Some 0xB0uy -> step (Convert(I64 IntOp.TruncF64,()) :: acc) !bytes
            | Some 0xB1uy -> step (Convert(U64 IntOp.TruncF64,()) :: acc) !bytes
            | Some 0xB2uy -> step (Convert(F32 FloatOp.ConvertSI32, ()) :: acc) !bytes
            | Some 0xB3uy -> step (Convert(F32 FloatOp.ConvertUI32, ()) :: acc) !bytes
            | Some 0xB4uy -> step (Convert(F32 FloatOp.ConvertSI64, ()) :: acc) !bytes
            | Some 0xB5uy -> step (Convert(F32 FloatOp.ConvertUI64, ()) :: acc) !bytes
            | Some 0xB6uy -> step (Convert(F32 FloatOp.DemoteF64, ()) :: acc) !bytes
            | Some 0xB7uy -> step (Convert(F64 FloatOp.ConvertSI32, ()) :: acc) !bytes
            | Some 0xB8uy -> step (Convert(F64 FloatOp.ConvertUI32, ()) :: acc) !bytes
            | Some 0xB9uy -> step (Convert(F64 FloatOp.ConvertSI64, ()) :: acc) !bytes
            | Some 0xBAuy -> step (Convert(F64 FloatOp.ConvertUI64, ()) :: acc) !bytes
            | Some 0xBBuy -> step (Convert(F64 FloatOp.PromoteF32, ()) :: acc) !bytes
            | Some 0xBCuy -> step (Spec(I32 IntOp.Reinterpret, ()) :: acc) !bytes
            | Some 0xBDuy -> step (Spec(I64 IntOp.Reinterpret, ()) :: acc) !bytes
            | Some 0xBEuy -> step (Spec(F32 FloatOp.Reinterpret, ()) :: acc) !bytes
            | Some 0xBFuy -> step (Spec(F64 FloatOp.Reinterpret, ()) :: acc) !bytes
            | Some 0xC0uy -> step (Spec(I32 (IntOp.ExtendS Pack8), ()) :: acc) !bytes
            | Some 0xC1uy -> step (Spec(I32 (IntOp.ExtendS Pack16), ()) :: acc) !bytes
            | Some 0xC2uy -> step (Spec(I64 (IntOp.ExtendS Pack8), ()) :: acc) !bytes
            | Some 0xC3uy -> step (Spec(I64 (IntOp.ExtendS Pack16), ()) :: acc) !bytes
            | Some 0xC4uy -> step (Spec(I64 (IntOp.ExtendS Pack32), ()) :: acc) !bytes

            | Some opcode   -> Error $"Opcode {opcode} not yet defined"
        step [] bytes


    let DecodeExpression bytes =
        match DecodeInstruction bytes with
        | Error msg -> Error msg
        | Ok(End(code, bytes)) -> Ok(Expr code,bytes)
        | Ok(Normal _) -> Error "Decoding error: Expression ending missing"
        | Ok(Else _)   -> Error "Decoding error: Expression ending mixup"
  

    let DecodeSection sectiontype pattern bytes =
        match %bytes with
        | None -> eof
        | Some idx when idx = byte sectiontype ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(0u, bytes) -> Ok(None, bytes)
            | Ok(sz, bytes) -> 
                // using take/skip to make sure that the size of the sections are correct
                // We use try .. with .. since they cast an expection on error
                try
                    match pattern (Seq.take (int sz) bytes) with
                    | Error msg -> Error msg
                    | Ok(ret, bytes') ->
                        if Seq.isEmpty bytes then
                            Ok(Some ret, bytes')
                        else
                            Error $"Section error: expected section size {sz} but it has still {Seq.length bytes - Seq.length bytes'}"
                with 
                    _ -> Error $"Section error: expected section size {sz} but found only {Seq.length bytes}"
                |> Result.map (fun ret -> fst ret, Seq.skip (int sz) bytes)
        
        | Some idx when idx > byte sectiontype -> Ok(None, bytes) // section missing on purpose
        | Some idx -> Error $"Section error: expected {sectiontype} but got {idx}"


    let DecodeTypeSection bytes =
        DecodeSection 1 (DecodeVecOf DecodeFunctionType) bytes

    let DecodeImportDescribtion bytes =
        match %bytes with
        | None -> eof
        | Some 0x00uy -> 
            DecodeU32 !bytes
            |> Result.map (fun (idx, bytes) -> ImFunc (int idx), bytes)

        | Some 0x01uy ->
            DecodeTableType !bytes
            |> Result.map (fun (tab, bytes) -> ImTable tab, bytes)

        | Some 0x02uy ->
            DecodeMemoryType !bytes
            |> Result.map (fun (mem, bytes) -> ImMemory mem, bytes)

        | Some 0x03uy ->
            DecodeGlobalType !bytes
            |> Result.map (fun (glb, bytes) -> ImGlobal glb, bytes)

        | _ -> Error "Expected a import header"


    let DecodeImport bytes =
        match DecodeName bytes with
        | Error msg -> Error msg
        | Ok(modname, bytes) ->
            match DecodeName bytes with
            | Error msg -> Error msg
            | Ok(name, bytes) ->
                DecodeImportDescribtion bytes
                |> Result.map (fun (imdesc, bytes) -> 
                    { modulename = modname; name = name; desc = imdesc }, bytes
                )
        
    let DecodeTable bytes =
        DecodeTableType bytes
        |> Result.map (fun (t, bytes) -> ({ ty = t } : table), bytes)

    let DecodeMemory bytes =
        DecodeMemoryType bytes
        |> Result.map (fun (t, bytes) -> { ty = t }, bytes)

    let DecodeImportSection bytes = DecodeSection 2 (DecodeVecOf DecodeImport) bytes
        
    let DecodeFunctionSection bytes = DecodeSection 3 (DecodeVecOf DecodeU32) bytes

    let DecodeTableSection bytes = DecodeSection 4 (DecodeVecOf DecodeTable) bytes

    let DecodeMemorySection bytes = DecodeSection 5 (DecodeVecOf DecodeMemory) bytes

    let DecodeGlobal bytes =
        match DecodeGlobalType bytes with
        | Error msg -> Error msg
        | Ok(gt, bytes) ->
            DecodeExpression bytes
            |> Result.map (fun (e, bytes) -> { ty = gt; name = ""; init = e }, bytes)
        

    let DecodeGlobalSection bytes = DecodeSection 6 (DecodeVecOf DecodeGlobal) bytes

    let DecodeExportDescribtion bytes =
        match %bytes with
        | None -> eof
        | Some 0x00uy -> 
            DecodeU32 !bytes
            |> Result.map (fun (idx, bytes) -> ExFunc (int idx), bytes)

        | Some 0x01uy ->
            DecodeU32 !bytes
            |> Result.map (fun (tab, bytes) -> ExTable (int tab), bytes)

        | Some 0x02uy ->
            DecodeU32 !bytes
            |> Result.map (fun (mem, bytes) -> ExMemory (int mem), bytes)

        | Some 0x03uy ->
            DecodeU32 !bytes
            |> Result.map (fun (glb, bytes) -> ExGlobal (int glb), bytes)

        | _ -> Error "Expected a export header"


    let DecodeExport bytes =
        match DecodeName bytes with
        | Error msg -> Error msg
        | Ok(name, bytes) ->
            DecodeExportDescribtion bytes
            |> Result.map (fun (desc, bytes) -> {name = name; desc = desc}, bytes)

    let DecodeExportSection bytes = DecodeSection 7 (DecodeVecOf DecodeExport) bytes

    let DecodeStartSection bytes = DecodeSection 8 DecodeU32 bytes
        

    let DecodeElement bytes =
        match %bytes with
        | None -> eof
        | Some 0x00uy ->
            match DecodeExpression !bytes with
            | Error msg -> Error msg
            | Ok(e, bytes) ->
                DecodeVecOf DecodeU32 bytes
                |> Result.map (fun (y, bytes) ->
                    let y = Expr(List.map (fun idx -> RefFunc(idx,())) y)
                    let mode = elemmode.Active {| index = 0; offset = e |}
                    { ty = refType.FuncRef; init= [y]; mode = mode }, bytes
                )

        | Some 0x01uy ->
             match %bytes with
             | None -> eof
             | Some 0x00uy -> 
                DecodeVecOf DecodeU32 !bytes
                |> Result.map (fun (y, bytes) ->
                    let y = Expr(List.map (fun idx -> RefFunc(idx,())) y)
                    { ty = refType.FuncRef; init= [y]; mode = elemmode.Passive }, bytes
                )

             | _ -> Error "Wrongfull element encoding"

        | Some 0x02uy ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(x, bytes) ->
                match DecodeExpression bytes with
                | Error msg -> Error msg
                | Ok(e, bytes) ->
                    match %bytes with
                    | None -> eof
                    | Some 0x00uy ->
                        DecodeVecOf DecodeU32 !bytes
                        |> Result.map (fun (y, bytes)->
                            let y = Expr(List.map (fun idx -> RefFunc(idx,())) y)
                            let mode = elemmode.Active {| index = int x; offset = e |}
                            { ty = refType.FuncRef; init= [y]; mode = mode }, bytes
                        )
                    | _ -> Error "Wrongfull element encoding"

        | Some 0x03uy ->
            match %bytes with
            | None -> eof
            | Some 0x00uy ->
                DecodeVecOf DecodeU32 !bytes
                |> Result.map (fun (y,bytes) -> 
                    let y = Expr(List.map (fun idx -> RefFunc(idx,())) y)
                    { ty = refType.FuncRef; init= [y]; mode = elemmode.Declarative }, bytes
                )
            | _ -> Error "Wrongfull element encoding"

        | Some 0x04uy ->
            match DecodeExpression !bytes with
            | Error msg -> Error msg
            | Ok(e, bytes) ->
                DecodeVecOf DecodeExpression bytes 
                |> Result.map (fun (el, bytes) ->
                    let mode = elemmode.Active {| index = 0; offset = e |}
                    { ty = refType.FuncRef; init= el; mode = mode }, bytes
                )

        | Some 0x05uy ->
            match DecodeRefType !bytes with
            | Error msg -> Error msg
            | Ok(et, bytes) ->
                DecodeVecOf DecodeExpression bytes
                |> Result.map (fun (el, bytes) ->
                    { ty = et; init = el; mode = elemmode.Passive }, bytes
                )

        | Some 0x06uy ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(x, bytes) ->
                match DecodeExpression bytes with
                | Error msg -> Error msg
                | Ok(e, bytes) ->
                    match DecodeRefType bytes with
                    | Error msg -> Error msg
                    | Ok(et, bytes) ->
                        DecodeVecOf DecodeExpression bytes
                        |> Result.map (fun (el, bytes) ->
                            let mode = elemmode.Active {| index = int x; offset = e |}
                            { ty = et; init = el; mode = mode }, bytes
                        )

        | Some 0x07uy ->
            match DecodeRefType !bytes with
            | Error msg -> Error msg
            | Ok(et, bytes) ->
                DecodeVecOf DecodeExpression bytes
                |> Result.map (fun (el, bytes) ->
                    { ty = et; init = el; mode = elemmode.Declarative }, bytes
                )

        | _ -> Error "Wrongfull element encoding"


    let DecodeElementSection bytes = DecodeSection 9 (DecodeVecOf DecodeElement) bytes


    let DecodeLocals bytes = 
        match DecodeU32 bytes with
        | Error msg -> Error msg
        | Ok(n, bytes) ->
            DecodeValueType bytes
            |> Result.map (fun (t, bytes) -> (n, t), bytes)

    let DecodeFunction bytes =
        match DecodeVecOf DecodeLocals bytes with
        | Error msg -> Error msg
        | Ok(locals, bytes) ->
            let locals = List.map (fun (id, t) -> int64 id, t) locals
            DecodeExpression bytes
            |> Result.map (fun (e, bytes) -> {ty = -1; name = ""; locals = locals; body = e }, bytes)

    let DecodeCode bytes =
        match DecodeU32 bytes with
        | Error msg -> Error msg
        | Ok(size, bytes) ->
            try
                let bytes' = Seq.take (int size) bytes
                match DecodeFunction bytes with
                | Error msg -> Error msg
                | Ok(code, bytes') ->
                    if Seq.isEmpty bytes' then
                        Ok(code, Seq.skip (int size) bytes)
                    else
                        Error "Wrong size parameter for code element in the code section"
            with
                _ -> Error $"Expected code to have size {size} but got {Seq.length bytes}"
                

    let DecodeCodeSection bytes = 
        DecodeSection 10 (DecodeVecOf DecodeCode) bytes
        

    let DecodeByte bytes =
        match %bytes with
        | None -> eof
        | Some b -> Ok(b, !bytes)

    let DecodeData bytes =
        match %bytes with
        | None -> eof
        | Some 0x00uy ->
            match DecodeExpression !bytes with
            | Error msg -> Error msg
            | Ok(e, bytes) ->
                DecodeVecOf DecodeByte bytes
                |> Result.map (fun (b, bytes) ->
                    let mode = datamode.Active {| index = 0; offset = e |}
                    { init = Array.ofList b; mode = mode }, bytes
                )                                  
                    
        | Some 0x01uy ->
            DecodeVecOf DecodeByte !bytes
            |> Result.map (fun (b, bytes) -> 
                { init = Array.ofList b; mode = datamode.Passive }, bytes
            )
            
        | Some 0x02uy ->
            match DecodeU32 !bytes with
            | Error msg -> Error msg
            | Ok(x, bytes) -> 
                match DecodeExpression bytes with
                | Error msg -> Error msg
                | Ok(e, bytes) ->
                    DecodeVecOf DecodeByte bytes
                    |> Result.map (fun (b, bytes) -> 
                        let mode = datamode.Active {| index = int x; offset = e |}
                        { init = Array.ofList b; mode = mode }, bytes
                    )
        | _ -> Error "Data encoding error"


    let DecodeDataSection bytes = DecodeSection 11 (DecodeVecOf DecodeData) bytes

    let DecodeDataCountSection bytes = DecodeSection 12 DecodeU32 bytes

    let DecodeMagic bytes =
        try 
            let b = Seq.take 4 bytes |> Array.ofSeq
            if b = [|0x00uy; 0x61uy; 0x73uy; 0x6Duy|] then
                Ok(Seq.skip 4 bytes)
            else
                Error "not a wasm file"
        with _ -> Error "not a wasm file"


    let DecodeVersion bytes =
        try 
            let b = Seq.take 4 bytes |> Array.ofSeq
            if b = [|0x01uy; 0x00uy; 0x00uy; 0x00uy|] then
                Ok(Seq.skip 4 bytes)
            else
                Error "wrong version"
        with _ -> Error "not a wasm file"


    let DecodeModule bytes =
        match DecodeMagic bytes with
        | Error msg -> Error msg
        | Ok bytes ->
            match DecodeVersion bytes with
            | Error msg -> Error msg
            | Ok bytes ->
                DecodeTypeSection bytes
                |> Result.map 
                    (fun (typesec, bytes) ->
                        { Module<_,_,_,_>.empty() with
                            types = Option.toList typesec |> List.concat
                        }, bytes
                    )
                |> Result.mapWithError  
                    (fun (Mod, bytes) ->
                        DecodeImportSection bytes
                        |> Result.map (fun (imports, bytes) ->
                            { Mod with imports = Option.toList imports |> List.concat }, bytes
                        )
                    )
                |> Result.mapWithError 
                    (fun (Mod, bytes) ->
                        DecodeFunctionSection bytes
                        |> Result.map (fun (fidxs, bytes) ->
                            let fidxs = Option.toList fidxs |> List.concat |> List.map int
                            Mod, fidxs, bytes
                        )
                    )
                |> Result.mapWithError 
                    (fun (Mod, idx, bytes) ->
                        DecodeTableSection bytes
                        |> Result.map (fun (tabs, bytes) ->
                            { Mod with
                                tables = Option.toList tabs |> List.concat
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError
                    (fun (Mod, idx, bytes) ->
                        DecodeMemorySection bytes
                        |> Result.map (fun (mems, bytes) ->
                            { Mod with
                                memories = Option.toList mems |> List.concat
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError
                    (fun (Mod, idx, bytes) ->
                        DecodeGlobalSection bytes
                        |> Result.map (fun (globals, bytes) ->
                            { Mod with
                                globals = Option.toList globals |> List.concat
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError
                    (fun (Mod, idx, bytes) ->
                        DecodeExportSection bytes
                        |> Result.map (fun (exports, bytes) ->
                            { Mod with
                                export = Option.toList exports |> List.concat
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError
                    (fun (Mod, idx, bytes) ->
                        DecodeStartSection bytes
                        |> Result.map (fun (start, bytes) ->
                            { Mod with
                                start = { func = start }
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError
                    (fun (Mod, idx, bytes) ->
                        DecodeElementSection bytes
                        |> Result.map (fun (elms, bytes) ->
                            { Mod with
                                elements = Option.toList elms |> List.concat
                            }, idx, bytes
                        )
                    )
                |> Result.mapWithError 
                    (fun (Mod, idx, bytes) ->
                        // we ignore the result
                        // it are used for parallel implementations
                        DecodeDataCountSection bytes
                        |> Result.map (fun (_, bytes) ->
                            Mod, idx, bytes
                        )
                    )
                |> Result.mapWithError 
                    (fun (Mod, idx, bytes) ->
                        // we ignore the result
                        // it are used for parallel implementations
                        DecodeCodeSection bytes
                        |> Result.mapWithError (fun (funs, bytes) ->
                            let funs = Option.toList funs |> List.concat
                            if funs.Length = idx.Length then
                                let funs = List.map2 (fun id func -> { func with ty = id }: func<_,_,_,_>) idx funs
                                Ok({ Mod with funcs = funs }, bytes)
                            else
                                Error "mismatch in number of functions types and elm in code section"
                        )
                    ) 
                |> Result.mapWithError
                    (fun (Mod, bytes) ->
                        // we ignore the result
                        // it are used for parallel implementations
                        DecodeDataSection bytes
                        |> Result.map (fun (data, bytes) ->
                            { Mod with
                                datas = Option.toList data |> List.concat
                            },bytes
                        )
                    )
                