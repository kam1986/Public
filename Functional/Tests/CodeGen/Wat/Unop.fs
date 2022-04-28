namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wat.Encoding

module Unop =
    
    [<TestClass>]
    type Unop() as test =

        [<TestMethod>]
        member _.UnOp() =
            let actual =
                let i = 
                    [ // sinec we have tested PrintOp we only need to test for operations not type
                        IntOp.Clz   
                        IntOp.Ctz   
                        IntOp.Popcnt                    
                    ] 
                    |> List.map I32
            
                let f =
                    [
                        FloatOp.Neg    
                        FloatOp.Abs    
                        FloatOp.Ceil   
                        FloatOp.Floor  
                        FloatOp.Trunc  
                        FloatOp.Nearest
                        FloatOp.Sqrt   
                    ]
                    |> List.map F32

                List.map PrintUnOp (i @ f)

            let expected =
                [
                    "i32.clz"
                    "i32.ctz"
                    "i32.popcnt"
                    "f32.neg"
                    "f32.abs"
                    "f32.ceil"
                    "f32.floor"
                    "f32.trunc"
                    "f32.nearest"
                    "f32.sqrt"
                ]

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member _.Test() =
            test.UnOp()

    