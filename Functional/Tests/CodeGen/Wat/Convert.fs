namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wat.Encoding

module Convert =
    
    [<TestClass>]
    type Convert() as test =
        
        [<TestMethod>]
        member _.Convert() = 
            let actual =
                [
                    I32 IntOp.WrapI64 
                    I32 IntOp.TruncF32
                    I32 IntOp.TruncF64
                    I64 IntOp.Extend  

                    F32 FloatOp.ConvertSI32
                    F32 FloatOp.ConvertUI32
                    F64 FloatOp.ConvertSI64
                    F64 FloatOp.ConvertUI64
                    F32 FloatOp.DemoteF64  
                    F64 FloatOp.PromoteF32 
                ]
                |> List.map PrintCvtOp

            let expected =
                [
                    "i32.wrap64"
                    "i32.trunc_f32"
                    "i32.trunc_f64"
                    "i64.extend_i32"
                    "f32.convert_i32_s"
                    "f32.convert_i32_u"
                    "f64.convert_i64_s"
                    "f64.convert_i64_u"
                    "f32.demote_f64"
                    "f64.promote_f32"
                ]

            expected = actual
            |> Assert.IsTrue
            

        [<TestMethod>]
        member _.Test() =
            test.Convert()

