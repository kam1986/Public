namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Wat.Encoding
open AbstractSyntax.Wasm
open Wasm
open Values

module Value =

    [<TestClass>]
    type Value() as test =
        
        [<TestMethod>]
        member _.Value() = 
            let expected = 
                [
                    "i32.const 1"
                    "i64.const 1"
                    "i32.const 1"
                    "i64.const 1"
                    "f32.const 1"
                    "f64.const 1"
                ]
            
            let actual =
                [
                    Const(I32 1)
                    Const(I64 1L)
                    Const(U32 1u)
                    Const(U64 1UL)
                    Const(F32 1f)
                    Const(F64 1.)
                ]
                |> List.map PrintInstruction
                |> List.concat
            
            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member _.Test() =
            test.Value()