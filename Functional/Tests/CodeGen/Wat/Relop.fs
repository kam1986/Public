namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wat.Encoding

module Relop =
    
    [<TestClass>]
    type Relop() as test =
        
        [<TestMethod>]
        member _.Relop() =
            let actual =
                let i = 
                    [
                        IntOp.Eq
                        IntOp.Ne
                        IntOp.Lt
                        IntOp.Gt
                        IntOp.Le
                        IntOp.Ge
                    ]
                    |> List.map I32

                let f =
                    [
                        FloatOp.Eq
                        FloatOp.Ne
                        FloatOp.Lt
                        FloatOp.Gt
                        FloatOp.Le
                        FloatOp.Ge
                    ]
                    |> List.map F32

                List.map PrintRelOp (i @ f)

            let expected =
                [
                    "i32.eq"
                    "i32.nq"
                    "i32.lt_s"
                    "i32.gt_s"
                    "i32.le_s"
                    "i32.ge_s"
                    "f32.eq"
                    "f32.nq"
                    "f32.lt"
                    "f32.gt"
                    "f32.le"
                    "f32.ge"
                ]



            expected = actual
            |> Assert.IsTrue
            

        [<TestMethod>]
        member _.Test() = 
            test.Relop()

