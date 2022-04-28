namespace CodeGen 
#nowarn "57"
module MemoryManagement =

    open Helpers

    open Helpers
    open SymTab
    open AbstractSyntax.Wasm
    open Wasm.Types
    open Wasm.Values

    let [<Literal>] StackPointer = "stack pointer"
    let [<Literal>] HeapPointer  = "heap pointer"
    let [<Literal>] Memsize      = "memory size"
    let [<Literal>] CurrentFrame = "current frame"
    let [<Literal>] LastFrame    = "last frame"
    let [<Literal>] currentAdr   = "stack"
    let [<Literal>] tempi32      = "temp i32"
    let [<Literal>] errno        = "errer number"


    let ENOMEM = Const(I32 1)

    // all functions below is souly for pointer arithmetic
    
        
    
