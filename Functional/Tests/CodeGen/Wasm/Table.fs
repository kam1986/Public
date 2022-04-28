module Wasm.Test.TableOperation

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type TableToBinary() as test =

    [<TestMethod>]
    member _.All() = 
        let actual =
            [        
                Load(Table  0, [])           
                Store(Table 0, [])          
                Tee(Table   0, [])  // added to make it make prior transformation easier     
                Init(Table  0, Table 1, [])  
                Drop(Table  0, [])          
                Copy(Table  0, Table 1, [])  
                Grow(Table  0, [])           
                Size(Table  0, [])           
                Fill(Table  0, [])           
            ]
            |> List.map GenInstruction

        let expected =
            [
                &0x25 => Idx 0
                &0x26 => Idx 0
                &0x26 => Idx 0 => &0x25 => Idx 0
                &0xFC => u32 12 => Idx 0 => Idx 1
                &0xFC => u32 13 => Idx 0
                &0xFC => u32 12 => Idx 0 => Idx 1
                &0xFC => u32 15 => Idx 0
                &0xFC => u32 16 => Idx 0
                &0xFC => u32 17 => Idx 0
            ]

        actual = expected
        |> Assert.IsTrue

    [<TestMethod>]
    member _.Test() = 
        test.All()
        

