// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

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


[<EntryPoint>]
let main argv =
    
    // let globals = [Int32_t, Global StackPointer;Int32_t, Global HeapPointer; Int32_t, Global CurrentFrame;Int32_t, Global Memsize ]
    // let names = [StackPointer; HeapPointer; CurrentFrame; Memsize]
    // let vtab = List.fold2 (fun tab name item -> Bind tab name item) (empty()) names globals
    // 
    // let one = Val(Int32 1)
    // let Two = Binop(Int32_t, binOp.Add, one, one)
    // 
    // let declare = Dec(Int32_t, "x", Two)
    // 
    // let ret = GenStatement pointersize 0 vtab (empty()) declare
    // 
    // printfn "%A" ret

    0 // return an integer exit code