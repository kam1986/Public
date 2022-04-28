module Wasm.Test.ParametricOperation


open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

type ParametricToBinary() as test = 
    
    member _.Drop() =
        
        let actual = GenInstruction (Drop(Stack,[]))
        
        let expected = &0x1A

        expected = actual
        |> Assert.IsTrue
        

    member _.Select() =

        let tys = [NumType I32_t; NumType I32_t]
    
        let actual =
            [
                Select([],[])
                Select(tys,[])
            ]
            |> List.map GenInstruction
        
        let expected =
            [
                &0x1B
                &0x1C => GenVec GenValueType tys 
            ]
        
        expected = actual
        |> Assert.IsTrue

     
    member _.Test() = 
        test.Drop()
        test.Select()