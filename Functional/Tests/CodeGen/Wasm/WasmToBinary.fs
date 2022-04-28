module WasmToBinary

open Wasm.Test
open Value
open Type
open ValueOperation
open ReferenceOperation
open ParametricOperation
open TableOperation
open VariableOperation
open MemoryOperation
open ControlOperation
open Module

type WasmToBinary() =
    
     
    member _.Test() =
        TypeToBinary().Test()
        ValueToBinary().Test()
        ReferenceToBinary().Test()
        ValueInstructionToBinary().Test()
        ParametricToBinary().Test()
        VariableToBinary().Test() 
        TableToBinary().Test()
        MemoryToBinary().Test()
        ControlToBinary().Test()
        ModuleToBinary().Test()