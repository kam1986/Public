module Wasm.Test.ReferenceOperation

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type ReferenceToBinary() as test =
    
    [<TestMethod>]
    member _.RefNullToBinary() = 
        let actual =
            [
                RefNull(FuncRef, [])
                RefNull(ExternRef, [])
            ]
            |> List.map GenInstruction

        let expected =
            [
                &0xD0 => GenRefType FuncRef
                &0xD0 => GenRefType ExternRef
            ]

        expected = actual
        |> Assert.IsTrue

        

    
    [<TestMethod>]
    member _.RefIsNulLToBinary() = 
        let actual =
            RefIsNull []
            |> GenInstruction


        let expected = &0xD1

        expected = actual
        |> Assert.IsTrue
        

    
    [<TestMethod>]
    member _.RefFuncToBinary() = 
        let actual = 
            RefFunc(0,[])
            |> GenInstruction

        let expected = &0xD2 => Idx 0
            
        expected = actual
        |> Assert.IsTrue
        

    
    [<TestMethod>]
    member _.Test() =
        test.RefNullToBinary()
        test.RefIsNulLToBinary()
        test.RefFuncToBinary()


