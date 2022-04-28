namespace C.Test
module Function =


    open System
    open Microsoft.VisualStudio.TestTools.UnitTesting


    // wasm
    open Wasm
    open Types
    open Values

    // intermediate C
    open AbstractSyntax.C
    open AbstractSyntax.Wasm
    open Encode

    open CodeGen.C
    open CodeGen.MemoryManagement

    open Helpers
    open SymTab

    [<TestClass>]
    type CFunctionToWasm () =
    
        [<TestMethod>]
        member _.Test() = ()
