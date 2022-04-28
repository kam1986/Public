module Wasm.Test.ControlOperation

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type ControlToBinary() as test =
    
    [<TestMethod>]
    member _.Unreachable() =
        let actual = 
            Unreachable []
            |> GenInstruction

        let expected = &0x00
        
        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Nop() =
        let actual = 
            Nop[]
            |> GenInstruction

        let expected = &0x01
        
        expected = actual
        |> Assert.IsTrue

    [<TestMethod>]
    member _.Br() = 
        let actual =
            Br (0, [])
            |> GenInstruction
        
        let expected =
            &0x0C => Idx 0
        
        expected = actual
        |> Assert.IsTrue

    [<TestMethod>]
    member _.BrIf() =
        let actual =
            BrIf (0, [])
            |> GenInstruction
        
        let expected =
            &0x0D => Idx 0
        
        expected = actual
        |> Assert.IsTrue

    [<TestMethod>]
    member _.If() =
        let bt = ValBlockType None

        let actual =
            [
                If(bt,[Nop[]],[],[])
                If(bt,[Nop[]],[Nop[]],[])
            ]
            |> List.map GenInstruction

        let expected =
            [
                &0x04 => GenBlockType bt => GenInstruction (Nop[]) => End
                &0x04 => GenBlockType bt => GenInstruction (Nop[]) => Else => GenInstruction (Nop[]) => End
            ]

        expected = actual
        |> Assert.IsTrue
        

    [<TestMethod>]
    member _.Loop() =
        let bt = ValBlockType None

        let actual =
            Loop(bt, [Nop[]], [])
            |> GenInstruction

        let expected =
            &0x03 => GenInstruction (Nop[]) => End

        expected = actual
        |> Assert.IsTrue
    
    [<TestMethod>]
    member _.Block() = 
        let bt = ValBlockType None
        
        let actual =
            Block(bt, [Nop[]], [])
            |> GenInstruction
        
        let expected =
            &0x02 => GenInstruction (Nop[]) => End
        
        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Test() =
        test.Unreachable()
        test.Nop()
        test.Br()
        test.BrIf()
        test.If()
        test.Loop()
        test.Block()