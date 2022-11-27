module MemoryManagement


(*
    
    This is a abtraction over memory management

    A memory model is basicly a list of thunks of continues memory.

*)

#nowarn "62"

open Syntax
open Table


// this will be tranlated into an offset for the fix sized array of smart or raw pointer in memory. 
type MemType =
    | Ty of Type
    | SmartPointer of SmartPointer
    | Arr of MemType * int


and Thunk =
    {
        fields: MemType[]
    }


// the memory is an abstraction over memory as a model
// layout 
(*
    header: 128 bits + memory
    header: size + first tunk of free memory. 
*)
and Memory = 
    {
        size: byte
        memory: Thunk list
        active: (string, SmartPointer) Table // lookup by owner in the table
    }



and SmartPointer =
    {
        // compiletime data
        shared : bool           // a compile time mark to determine from when the data is shared across threads, it will affect which instructions generation. 
        byMutex: bool           // indicate if the data is shared by mutex over atomic a compile time header
        owner: string           // can kill it even when there exist references 
        reference: string list  // reference to this thunk from other thunks 
        // runtime data
        header: byte            // mask for things like if it is still alive, locked by mutex
        size: uint
        data: Thunk             // this will be translated into an offset into main memory
    }

