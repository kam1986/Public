module TextModule

open Wasm
open Types
open Values

(*

    This is an easy way to map identifiers to thing
    
*)

type 'a vec = Map<string, 'a>

type TextModule<'id, 'fid, 'mem, 'info> =
    {
        name:       Vec<funcType>
        types:      Vec<func<'id, 'fid, 'mem, 'info>>
        funcs:      Vec<table>
        tables:     Vec<memory>
        memories:   Vec<Global<'id, 'fid, 'mem, 'info> >
        globals:    Vec<element<'id, 'fid, 'mem, 'info> >
        elements:   Vec<data<'id, 'fid, 'mem, 'info> >
        import:     Vec<import>
        export:     Vec<export>
        start:     'fid Start
    }