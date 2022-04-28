(*
    Assume that i32, i64, u32, and u64 all has been tested prior to this
*)

module Wasm.Test.MemoryOperation


open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type MemoryToBinary() as test = 
    
    [<TestMethod>]
    member _.Memarg() =
        let actual = 
            { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, SX)} 
            |> GenMemArg

        let expected = u32 2 => u32 0

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Load() = 
        let actual = 
            [
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, SX)}  
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, ZX)}  
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, SX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, ZX)} 
                
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, SX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, ZX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = None}

                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, SX)}  
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, ZX)}  
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, SX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, ZX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, SX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, ZX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = None} 

                { ty = F32_t; align = 2; name = 0; offset = 0; sz = None} 
                { ty = F64_t; align = 2; name = 0; offset = 0; sz = None} 
                

            ]
            |> List.map (fun memop -> Load(Memory memop,[]))
            |> List.map GenInstruction
        
        let expected =
            [
                // i32
                &0x2C => u32 2 => u32 0
                &0x2D => u32 2 => u32 0
                &0x2E => u32 2 => u32 0
                &0x2F => u32 2 => u32 0
                // should be the same for these 3 cases
                &0x28 => u32 2 => u32 0
                &0x28 => u32 2 => u32 0
                &0x28 => u32 2 => u32 0
                
                // i64
                &0x30 => u32 2 => u32 0
                &0x31 => u32 2 => u32 0
                &0x32 => u32 2 => u32 0
                &0x33 => u32 2 => u32 0
                &0x34 => u32 2 => u32 0
                &0x35 => u32 2 => u32 0
                &0x29 => u32 2 => u32 0

                &0x2A => u32 2 => u32 0
                &0x2B => u32 2 => u32 0
            ]
        
        expected = actual
        |> Assert.IsTrue
        
        

    [<TestMethod>]
    member _.Store() = 
        let actual = 
            [
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, SX)}  
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, ZX)}  
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, SX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, ZX)} 
                
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, SX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, ZX)} 
                { ty = I32_t; align = 2; name = 0; offset = 0; sz = None}
                
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, SX)}  
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack8, ZX)}  
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, SX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack16, ZX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, SX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = Some(Pack32, ZX)} 
                { ty = I64_t; align = 2; name = 0; offset = 0; sz = None} 
                
                { ty = F32_t; align = 2; name = 0; offset = 0; sz = None} 
                { ty = F64_t; align = 2; name = 0; offset = 0; sz = None} 
            ]
            |> List.map (fun memop -> Store(Memory memop,[]))
            |> List.map GenInstruction


        let expected = 
            [
                // i32
                &0x3A => u32 2 => u32 0
                &0x3A => u32 2 => u32 0
                &0x3B => u32 2 => u32 0
                &0x3B => u32 2 => u32 0
                // should be the same for both cases
                &0x36 => u32 2 => u32 0
                &0x36 => u32 2 => u32 0
                &0x36 => u32 2 => u32 0
                // i64
                &0x3C => u32 2 => u32 0
                &0x3C => u32 2 => u32 0
                &0x3D => u32 2 => u32 0
                &0x3D => u32 2 => u32 0
                &0x3E => u32 2 => u32 0
                &0x3E => u32 2 => u32 0
                &0x37 => u32 2 => u32 0
                
                &0x38 => u32 2 => u32 0
                &0x39 => u32 2 => u32 0
            ]

        expected = actual
        |> Assert.IsTrue
        
    
    [<TestMethod>]
    member _.Other() = 
        let memop = Memory { ty = I32_t; align = 2; name = 0; offset = 0; sz = None}
        let actual = 
            [
                Size(memop, [])
                Grow(memop, [])
                Drop(memop, [])
                Init(memop, memop, [])
                Copy(memop, memop, [])
                Fill(memop, [])
            ]
            |> List.map GenInstruction

        let expected = 
            [
                &0x3F => &0x00
                &0x40 => &0x00
                &0xFC => u32 9 => Idx 0
                &0xFC => u32 8 => Idx 0 => &0x00
                &0xFC => u32 10 => &0x00 => &0x00
                &0xFC => u32 11 => &0x00 
            ]

        expected = actual
        |> Assert.IsTrue
        
    
    [<TestMethod>]
    member _.Test() = 
        test.Memarg()
        test.Load()
        test.Store()
        test.Other()