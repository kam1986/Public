namespace Wat


open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Wat.Encoding

open Wasm.Types

module Type =
    
    [<TestClass>]
    type Type() as test =
        
        [<TestMethod>]
        member _.Type() =
            let actual1 =
                [
                    I32_t
                    I64_t
                    F32_t
                    F64_t
                ]
                |> List.map PrintValType

            let expected1 =
                [
                    "i32"
                    "i64"
                    "f32"
                    "f64"
                ]
            

            let actual2 = 
                [
                    FuncRef
                    ExternRef
                ]
                |> List.map PrintRefType

            let expected2 =
                [
                    "funcref"
                    "externref"
                ]

            expected1 = actual1
            |> Assert.IsTrue

            expected2 = actual2
            |> Assert.IsTrue



        [<TestMethod>]
        member _.Test() =
            test.Type()

