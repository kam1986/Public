module TypeAcrobatics

(*

    OBS!!! this should at some point be transformed into an interface 'I value with

    tryParse and parse methods where the token type implement it for all basic types

*)

/// Abstraction that hide the type from the type system.
/// enabling collection of none uniform types into the same collection without interfaces
type Type = Type of obj

let inline internal AnyType t = Type (t :> obj)

// carry argument as a postpond transformation
// i.e. the type checking is first done, at run time. 
type token = Arg of (unit -> Type)


let inline internal Delay work input = 
    fun _ -> work input |> AnyType
    |> Arg 

let internal Arg item = Delay id item


/// This safely cast the the type
/// will throw an error if the return value are not used properly
/// example:
///     let One = Arg 1
///     printfn "%d" <| Take One // here the expected input of printfn is int do To '%d' it also do fine with %A
let inline internal Take (Arg a) = 
    let (Type t) = a()
    t :?> _ // runtime check i.e. performance penalty

