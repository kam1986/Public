namespace C.Test

module Condition =
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
    type CCondToWasm () as Test =
        let empty() = (DynamicTable<_,_>.Empty() :> ISymbolic<_,_>)

        let GenCond = GenCond 0 (empty()) (empty())
        let GenExpr = GenExpr 0 (empty()) (empty())

        [<TestMethod>]
        member private _.GenTrue () =
            let actual = GenCond True
            let expected = Ok(&Const(I32 1))

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member private _.GenFalse () =
            let actual = GenCond False
            let expected = Ok(&Const(I32 0))

            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenRelOp() =
            let one = Val(Float64 1.)
            let two = Val(Float64 2.)
            let actual =
                Relop(Float64_t, relOp.Eq, one, two)
                |> GenCond 

            let expected = GenExpr one <+> GenExpr two <+> Ok(&Compare(F64 FloatOp.relop.Eq, [Int32_t]))
        
            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenAnd() =
            let actual = 
                And(True, True)
                |> GenCond

            let expected = GenCond True <+> Ok(&If(ValBlockType (Some (NumType I32_t)), &Const(I32 1), &Const(I32 0) ,[Int32_t]))

            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenOr() =
            let actual = 
                Or(False, False)
                |> GenCond

            let expected = GenCond False <+> Ok(&If(ValBlockType (Some (NumType I32_t)), &Const(I32 1), &Const(I32 0) ,[Int32_t]))

            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenNot() =
            let actual = 
                Not(False)
                |> GenCond

            let expected = GenCond False <+> Ok(&instr.Test(I32 IntOp.testop.Eqz, [Int32_t]))

            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Test() = 
            Test.GenTrue()
            Test.GenFalse()
            Test.GenRelOp()
            Test.GenAnd()
            Test.GenOr()
            Test.GenNot()