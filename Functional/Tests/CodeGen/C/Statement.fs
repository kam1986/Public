namespace C.Test
module Statement =

    open System
    open Microsoft.VisualStudio.TestTools.UnitTesting


    // wasm
    open Wasm
    open Types
    open Values

    // intermediate C
    open AbstractSyntax.C
    open AbstractSyntax.Wasm
    open Encode

    open CodeGen.C
    open CodeGen.MemoryManagement

    open Helpers
    open SymTab

    [<TestClass>] 
    type CStatementToWasm () as Test =
    
        let vtab = DynamicTable<_,_>.Empty() :> ISymbolic<_,_>
        let ftab = DynamicTable<_,_>.Empty() :> ISymbolic<_,_>

   

        member private _.GenDec() =  
            let loc = Memory { ty = I32_t; name = "x"; offset = 0; align = 2; sz = Some(Pack32,SX) }
            let actual_vtab, actual_code =
                Dec(Int32_t, "x", Val(Int32 1))
                |> GenStatement 0 0 vtab ftab 

            let expected_vtab = Bind vtab "x" (Int32_t, loc) 
            let expected_code = GenExpr 0 vtab ftab (Val(Int32 1)) <+> Ok(&Load(Global CurrentFrame, [pointertype])) <+> Ok(&Store(loc,[Int32_t]))
        
            expected_vtab = snd actual_vtab
            |> Assert.IsTrue

            expected_code = actual_code
            |> Assert.IsTrue
    
        member private _.GenAssign() =
            let vtab = Bind vtab "x" (Int32_t, Memory { ty = I32_t; name = "x"; offset = 0; align = 2; sz = None })
            let actual_vtab, actual_code =
                Assign("x", Val(Int32 1))
                |> GenStatement 0 0 vtab ftab 

            let expected_vtab = Bind (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>) "x" (Int32_t, Memory { ty = I32_t; name = "x"; offset = 0; align = 2; sz = None })
            let expected_code = GenExpr 0 vtab ftab (Val(Int32 1)) <+> Ok(&Store(Memory { ty = I32_t; name = "x"; offset = 0; align = 2; sz = None },[Int32_t]))

            expected_vtab = snd actual_vtab
            |> Assert.IsTrue
          
            expected_code = actual_code
            |> Assert.IsTrue
    

        member private _.GenIf() = 
            let cond = True
            let b = Expr.Val (Int32 1)
        
            let vtab', actual = 
                Statement.If(cond, Statement.Return b, Statement.Return b)
                |> GenStatement 0 0 vtab ftab

            let expected =
                 let bcode = 
                    GenStatement 0 0 vtab ftab (Statement.Return b) 
                    |> snd
                    |> Result.map (fun code ->  &If(ValBlockType None, code, code, []))

                 let ccode = GenCond 0 vtab ftab cond

                 ccode <+> bcode


            vtab = snd vtab'
            |> Assert.IsTrue

            expected = actual
            |> Assert.IsTrue

    
        member private _.GenWhile() = 
            let GenStatement = GenStatement 0 0 vtab ftab
        
            let cond = True
            let body = Statement.Return (Val (Int32 1))
            let vtab', actual = 
                While(cond, body, Some Int32_t)
                |> GenStatement

            let vtab'', body = GenStatement body

            let ty = Option.toList (Some Int32_t)
            let expected =
                let cond = GenCond 0 vtab ftab cond
                let bcode = 
                    body <+> cond
                    |> Result.map (fun body ->
                        &Block(
                            ValBlockType None, 
                            &Loop(
                                ValBlockType None, 
                                seq[yield! body; BrIf("1",ty)] |> List.ofSeq, 
                                ty
                            ),
                            ty
                       )
                    )
                cond <+> bcode
        
            (vtab = snd vtab' && vtab = snd vtab'')
            |> Assert.IsTrue

            expected = actual
            |> Assert.IsTrue

    
        member private _.GenBreak() = 
            let vtab', actual1 = GenStatement 0 2 vtab ftab Break
            let vtab'', actual2 = GenStatement 0 0 vtab ftab Break

            let expected1 = &Br("0", []) |> Ok
            let expected2 = Error "Not in a loop"
        
            (snd vtab' = vtab && snd vtab'' = vtab)
            |> Assert.IsTrue

            expected1 = actual1
            |> Assert.IsTrue
        
            expected2 = actual2
            |> Assert.IsTrue


        member private _.GenContinue() =
            let vtab', actual1 = GenStatement 0 2 vtab ftab Continue
            let vtab'', actual2 = GenStatement 0 0 vtab ftab Continue

            let expected1 = &Br("1", []) |> Ok
            let expected2 = Error "Not in a loop"

            (snd vtab' = vtab && snd vtab'' = vtab)
            |> Assert.IsTrue

            expected1 = actual1
            |> Assert.IsTrue
        
        
            actual2 = expected2       
            |> Assert.IsTrue
        
    
    
        member private _.GenReturn() = 
            let vtab', actual = GenStatement 0 0 vtab ftab (Statement.Return (Val(Int32 1)))
            let expected = Ok(&Const(I32 1)) <+> Ok(&Return([])) 

            snd vtab' = vtab
            |> Assert.IsTrue

            expected = actual
            |> Assert.IsTrue
    

        member private _.GenSequence() = 
        
            let first = Dec(Int32_t, "x",Val(Int32 1)) // is tested above
            let second = Statement.Assign("x", Binop(Int32_t, binOp.Add, Var "x", Val(Int32 2))) // testing transference of binding

            let actualvtab = Bind vtab "x" (Int32_t, Memory { ty = I32_t; name = "x"; offset = 0; align = 2; sz = Some(Pack32, SX) })

            let vtab', actual_code = 
                Sequence(first, second)
                |> GenStatement 0 0 vtab ftab 

            // assume that the tests above are correct, and uses the induction property of recursion
            let (offset, vtab''), first' = GenStatement 0 0 vtab ftab first
            let vtab''', second' = GenStatement offset 0 vtab'' ftab second 
            let expected_code = first' <+> second'

            // testing that the vtab are updated correctly through the call
            (actualvtab = snd vtab' && actualvtab = vtab'' && actualvtab = snd vtab''')
            |> Assert.IsTrue

        
            expected_code = actual_code
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Test() =
            Test.GenDec()
            Test.GenAssign()
            Test.GenIf()
            Test.GenWhile()
            Test.GenBreak()
            Test.GenContinue()
            Test.GenReturn()
            Test.GenSequence()
