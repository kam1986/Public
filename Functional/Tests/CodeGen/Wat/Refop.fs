namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wat.Encoding

module Refop =
    
    [<TestClass>]
    type Refop() as test =
        
        [<TestMethod>]
        member _.RefOp() =
            let actual =  
                [
                    RefNull (refType.FuncRef, [])
                    RefIsNull([])
                    RefFunc(0,[])
                ]
                |> List.map PrintInstruction
                |> List.concat


            let expected = 
                [
                    "ref.null funcref"
                    "ref.is_null"
                    "ref.func 0"
                ]

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member _.Test() = 
            test.RefOp()

