namespace C.Test
(*
    
    This is the test class for testing code generation of types in the
    intermediate language C to wasm

    We simply testing one function, that call the simple function recursively
    on a list by using the List.map function.

*)


module Type =

    open System
    open Microsoft.VisualStudio.TestTools.UnitTesting

    open SymTab
    open AbstractSyntax.C
    open Wasm.Types
    open CodeGen.C

    [<TestClass>]
    type CTypeToWasmType() as Test = 
    
        [<TestMethod>]
        member _.GenType() =
            let tps =
                [
                    Int8_t
                    Int16_t
                    Int32_t
                    Int64_t
                    UInt8_t
                    UInt16_t
                    UInt32_t
                    UInt64_t
                    Float32_t
                    Float64_t
                ]

            let expected = 
                [
                    I32_t
                    I32_t
                    I32_t
                    I64_t
                    I32_t
                    I32_t
                    I32_t
                    I64_t
                    F32_t
                    F64_t
                ]
        
            let actual = GenTypes tps

            expected = actual
            |> Assert.IsTrue


        member _.Test() =
            Test.GenType()