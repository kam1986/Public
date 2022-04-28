namespace C.Test

#nowarn "25"
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

module Value =

    // wasm
    open Wasm
    open Types
    open Values

    // intermediate C
    open AbstractSyntax.C
    open AbstractSyntax.Wasm

    open CodeGen.C

    [<TestClass>]
    type CValueToWasmValue() as Test =
        // used in multiple tests
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

        [<TestMethod>]
        member private _.GenZero() = 

            let actual = 
                List.map GenZero tps
                // the function GenZero will never return Error
                // and there will be only one instruction in the sequence
                |> List.map (function Ok(s) -> Seq.head s)

            let expected =
                [
                    Const (I32 0)
                    Const (I32 0)
                    Const (I32 0)
                    Const (I64 0L)
                    Const (U32 0u)
                    Const (U32 0u)
                    Const (U32 0u)
                    Const (U64 0UL)
                    Const (F32 0f)
                    Const (F64 0.)
                ]

            actual = expected
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenOne() = 
            let actual = 
                List.map GenOne tps                
                // the function GenOne will never return Error
                // and there will be only one instruction in the sequence
                |> List.map (function Ok(s) -> Seq.head s)

            let expected =
                [
                    Const (I32 1)
                    Const (I32 1)
                    Const (I32 1)
                    Const (I64 1L)
                    Const (U32 1u)
                    Const (U32 1u)
                    Const (U32 1u)
                    Const (U64 1UL)
                    Const (F32 1f)
                    Const (F64 1.)
                ]

            actual = expected
            |> Assert.IsTrue


        member private _.GenValue() =
            let actual =
                [
                    Int8     1y
                    Int16    1s
                    Int32    1
                    Int64    1L
                    UInt8    1uy
                    UInt16   1us
                    UInt32   1u
                    UInt64   1UL
                    Float32  1f
                    Float64  1.
                ]
                |> List.map GenValue
                |> List.map (function Ok v -> v)
        
            let expected =
                [
                    I32 1
                    I32 1
                    I32 1
                    I64 1L
                    U32 1u
                    U32 1u
                    U32 1u
                    U64 1UL
                    F32 1f
                    F64 1.
                ]

            actual = expected
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenSign() =
            let actual = 
                [
                    Int32_t
                    UInt32_t
                    Float32_t
                ]
                |> List.map GenSign

            let expected =
                [
                    Some SX
                    Some ZX
                    None
                ]

            actual = expected
            |> Assert.IsTrue


        [<TestMethod>]
        member private _.GenPackSize() =
            let actual = 
                [
                  Int8_t
                  UInt16_t
                  Int32_t
                  UInt64_t
                  Float64_t  
                ]
                |> List.map GenPackSize

            let expected = 
                [
                    Some Pack8
                    Some Pack16
                    Some Pack32
                    None
                    None
                ]

            actual = expected
            |> Assert.IsTrue



        [<TestMethod>]
        member _.Test() =
            Test.GenZero()
            Test.GenOne()
            Test.GenValue()
            Test.GenSign()