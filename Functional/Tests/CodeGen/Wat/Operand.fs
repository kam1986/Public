namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wat.Encoding

module Operand =
    
    [<TestClass>]
    type Operand() as test =
        
        [<TestMethod>]
        member _.Operand() =
            let actual =
                [
                    I32()
                    I64()
                    U32()
                    U64()
                    F32()
                    F64()
                ]
                |> List.map PrintOp
            
            let expected =
                [
                    "i32."
                    "i64."
                    "i32."
                    "i64."
                    "f32."
                    "f64."
                ]
            
            expected = actual
            |> Assert.IsTrue

        // to minimize testing for relop and binop
        [<TestMethod>]
        member _.Sign() =
            let actual =
                [
                    I32()
                    I64()
                    U32()
                    U64()
                    F32()
                    F64()
                ]
                |> List.map PrintSign

            let expected =
                [
                    "_s"
                    "_s"
                    "_u"
                    "_u"
                    ""
                    ""
                ]

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member _.Test() = 
            test.Operand()
            test.Sign()

