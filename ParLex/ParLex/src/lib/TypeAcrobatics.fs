module TypeAcrobatics

type Type = Type of obj

let inline internal AnyType t = Type (t :> obj)

type token = Arg of (unit -> Type)


let inline internal Delay work input = 
    fun _ -> work input |> AnyType
    |> Arg 

let internal Arg item = Delay id item


let inline internal Take (Arg a) = 
    let (Type t) = a()
    t :?> _ // runtime check i.e. performance penalty

