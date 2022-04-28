namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wat.Encoding


module Binop =

    [<TestClass>]
    type Binop() as test =
        
        [<TestMethod>]
        member _.Binop() = 
            let actual =
                let i = 
                    [
                        IntOp.Add 
                        IntOp.Sub 
                        IntOp.Mul 
                        IntOp.Div 
                        IntOp.Rem 
                        IntOp.And 
                        IntOp.Or  
                        IntOp.Xor 
                        IntOp.Shl 
                        IntOp.Shr 
                        IntOp.Rotl
                        IntOp.Rotr
                    ]
                    |> List.map U64

                let f =
                    [
                        FloatOp.Add     
                        FloatOp.Sub     
                        FloatOp.Mul     
                        FloatOp.Div     
                        FloatOp.Min     
                        FloatOp.Max     
                        FloatOp.CopySign
                    ]
                    |> List.map F64
                
                List.map PrintBinOp (i @ f)

            let expected =
                [
                    "i64.add"
                    "i64.sub"
                    "i64.mul"
                    "i64.div_u"
                    "i64.rem_u"
                    "i64.and"
                    "i64.or"
                    "i64.xor"
                    "i64.shl"
                    "i64.shr_u"
                    "i64.rotl"
                    "i64.rotr"
                    "f64.add"
                    "f64.sub"
                    "f64.mul"
                    "f64.div"
                    "f64.min"
                    "f64.max"
                    "f64.copysign"
                ]

            expected = actual
            |> Assert.IsTrue
            ()


        [<TestMethod>]
        member _.Test() =
            test.Binop()

