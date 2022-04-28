namespace C.Test
(*

    This is the test Class for code generation from C Expressions
    We assume that symboltable has been tested beforehand and will
    function correctly. See Tests/Data Types/SymbolTable.fs for the test
    for the symboltable

    Since All functions either has no mutable state and work by recursion
    We can simply test the base cases, since it by induction over recursion
    follows that any thing build up from base cases is again correct if 
    the base cases are correct.

    We leave out the 'Cond c' case since it is just a mutual recursive call to
    the GenCondition function which is tested in Condition.fs.
    

    TODO:
        Change the GenCall test when memory model are done 
*)
module Expression =

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
    type CExprToWasm () as Test =

        let globals = [Int32_t, Global StackPointer; Int32_t, Global HeapPointer; Int32_t, Global CurrentFrame; Int32_t, Global Memsize ]
        let names = [StackPointer; HeapPointer; CurrentFrame; Memsize]
        let vtab = List.fold2 (fun tab name item -> Bind tab name item) (DynamicTable<_,_>.Empty() :> _) names globals

        let one = Val(Int32 1)
        let two = Binop(Int32_t, binOp.Add, one, one)

        [<TestMethod>]
        member private _.GenValue() =
        
            let actual =
                [
                    Val(Int32 1)
                ]
                |> List.map (GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>))
                |> List.map (function Ok code -> code | Error msg -> failwith msg)
                |> List.map List.ofSeq
                |> List.concat

            let expected =
                [
                    Const(I32 1)
                ]

            expected = actual
            |> Assert.IsTrue

    
        [<TestMethod>]
        member private _.GenVariable() =
            let actual =
                [
                    Var StackPointer
                ]
                |> List.map (GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>))
                |> List.map (function Ok code -> code | Error msg -> failwith msg)
                |> List.map List.ofSeq
                |> List.concat

            let expected =
                [
                    Load(Global StackPointer, [Int32_t])
                ]

            expected = actual
            |> Assert.IsTrue

        // since we have tested for Operation Generation it is enough
        // to test for one case
        [<TestMethod>]
        member private _.GenBinOp() =
            let one = Val(Int32 1)
            let one' = Result.map ((~&) << Const) (i32 1)
        
            let actual = 
                Binop(Int32_t, binOp.Add, one, one)
                |> GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>)

            let expected = one' <+> one' <+> Ok(&Binary(I32 IntOp.binop.Add,[Int32_t]))

            expected = actual
            |> Assert.IsTrue


        // since we have tested for Operation Generation it is enough
        // to test for two cases
        [<TestMethod>]
        member private _.GenUnOp() =
            // case 1
            let one = Val(Int32 1)
            let one' = Result.map ((~&) << Const) (i32 1)
            let zero' = Result.map ((~&) << Const) (i32 0)

            let actual1 = 
                Unop(Int32_t, unOp.Neg,one)
                |> GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>)

            let expected1 = zero' <+> one' <+> Ok(&Binary(I32 IntOp.binop.Sub, [Int32_t]))


            // case 2
            let one = Val(Float32 1f)
            let one' = Result.map ((~&) << Const) (f32 1)

            let actual2 = 
                Unop(Float32_t, unOp.Neg,one)
                |> GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>)

            let expected2 = one' <+> Ok(&Unary(F32 FloatOp.unop.Neg, [Float32_t]))

            expected1 = actual1
            |> Assert.IsTrue

            expected2 = actual2
            |> Assert.IsTrue


        member private _.GenIf() =
            let t = Val(Int32 1)
            let f = Val(Int32 0)

            let i = Expr.If(Int32_t, Cond.True, t, f)

            let actual = GenExpr 0 vtab (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>) i
            let expected = Const(I32 1) +> Ok(&If(ValBlockType(Some(NumType I32_t)), [Const(I32 1)], [Const(I32 0)], [Int32_t]))
            
            expected = actual
            |> Assert.IsTrue

        // we have not included local declarations yet
        // since the memory model aren't done yet
        // and this will have an effect on the test
        member private _.GenCall() =
            let ftab = Bind (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>) "f" Int32_t

            let one = Val(Int32 1)
            let actual =
                Expr.Call(Int32_t, "f", [one])
                |> GenExpr 0 vtab ftab

            let (_, vtab), setframe =
                Dec(pointertype, "tmp stack pointer", Var CurrentFrame)                                                // push current frame top pointer to the stack. 
                --> Assign(CurrentFrame, Var StackPointer)                                                              // set new frame pointer into current fram pointer
                --> Assign(StackPointer, Binop(pointertype, binOp.Add, Var StackPointer, Val(pointerop pointersize)))   // update stackpointer
                |> GenStatement 0 0 vtab ftab   

            let _, endframe =
                Assign(StackPointer, Var CurrentFrame)
                --> Assign(CurrentFrame, Var "tmp stack pointer")
                |> GenStatement 0 0 vtab ftab

            let expect =        
                GenExpr 0 vtab ftab one <+> setframe <+> Ok(&Call("f", [Int32_t])) <+> endframe
        
            expect = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Test() =
            Test.GenValue()
            Test.GenVariable()
            Test.GenBinOp()
            Test.GenUnOp()
            Test.GenIf()
            Test.GenCall()