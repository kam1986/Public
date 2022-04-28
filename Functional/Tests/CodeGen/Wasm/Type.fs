module Wasm.Test.Type

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm
open Types
open Wasm.Encode


[<TestClass>]
type TypeToBinary() as test =
    

    [<TestMethod>]
    member _.NumType() =
        let actual =
            [
                I32_t
                I64_t
                F32_t
                F64_t
            ]
            |> List.map GenNumType

        let expected = 
            [
                &0x7F
                &0x7E
                &0x7D
                &0x7C
            ]

        expected = actual
        |> Assert.IsTrue
        

    [<TestMethod>]
    member _.RefType() =
        let actual =
            [
                FuncRef  
                ExternRef
            ]
            |> List.map GenRefType

        let expected =
            [
                &0x70
                &0x6F
            ]

        expected = actual
        |> Assert.IsTrue
        

    [<TestMethod>]
    member _.ValueType() =
        let actual =
            [
                NumType I32_t
                RefType FuncRef  
            ]
            |> List.map GenValueType

        let expected =
            [
                &0x7F
                &0x70
            ]

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.ResultType() =
        let actual = 
            [
                NumType I32_t
                RefType FuncRef  
            ]
            |> Result
            |> GenResulType

        let expected = u32 2 => GenNumType I32_t => GenRefType FuncRef

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.FunctionType() =
        let actual = GenFuncType (FuncType(Result [NumType I32_t], Result[NumType I32_t]))

        let ty = u32 1 => GenNumType I32_t
        let expected = &0x60uy => ty => ty

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Limits() =
        let actual0 = GenLimits { min = 0; max = None; share = Shared} 
        let actual1 = GenLimits { min = 0; max = Some 5000; share = Shared}

        let expected0 = &0x00uy => u32 0
        let expected1 = &0x01uy => u32 0 => u32 5000


        expected0 = actual0
        |> Assert.IsTrue

        expected1 = actual1
        |> Assert.IsTrue


    [<TestMethod>]
    member _.MemoryType() = 
        let actual = GenMemType (MemoryType { min = 0; max = None; share = Shared})
        let expected = GenLimits { min = 0; max = None; share = Shared} 

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.GlobalType() =
        let actual = GenGlobalType (GlobalType(NumType I32_t, Immutable))
        let expected = GenNumType I32_t => &0x00uy

        expected = actual
        |> Assert.IsTrue
    
    [<TestMethod>]
    member _.TableType() = 
        let limit = {min = 0; max = None; share = Unshared}
        let actual = GenTableType (TableType(limit, FuncRef))
        let expected = GenRefType FuncRef => GenLimits limit

        expected = actual
        |> Assert.IsTrue

    [<TestMethod>]
    member _.BlockType() =
        let actual =
            [
                ValBlockType None
                ValBlockType (Some (NumType I32_t))
                VarBlockType 55
            ]
            |> List.map GenBlockType

        let expected =
            [
                seq[]
                GenValueType (NumType I32_t)
                i64 55 :> _ seq
            ]


        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Test() = 
        test.NumType()
        test.RefType()
        test.ValueType()
        test.ResultType()
        test.FunctionType()
        test.Limits()
        test.MemoryType()
        test.GlobalType()
        test.BlockType()