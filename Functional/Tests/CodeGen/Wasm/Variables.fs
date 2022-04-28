module Wasm.Test.VariableOperation

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type VariableToBinary() as test =
    [<TestMethod>]
    member _.Local() = 
        let loc = Local 0
        let actual = 
            [
               Load(loc, [])
               Store(loc, [])
               Tee(loc, [])
            ]
            |> List.map GenInstruction

        let expected =
            [
                &0x20 => Idx 0
                &0x21 => Idx 0
                &0x22 => Idx 0
            ]

        expected = actual
        |> Assert.IsTrue
        
    
    [<TestMethod>]
    member _.Global() = 
        let loc = Global 0
        let actual = 
            [
               Load(loc, [])
               Store(loc, [])
               Tee(loc, [])
            ]
            |> List.map GenInstruction
        
        let expected =
            [
                &0x23 => Idx 0
                &0x24 => Idx 0
                &0x24 => Idx 0 => &0x23 => Idx 0
            ]

        expected = actual
        |> Assert.IsTrue        
        

    [<TestMethod>]
    member _.Test() = 
        test.Local()
        test.Global()