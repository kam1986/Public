module TypeAcrobatics

type Type = Value of obj

type value = Work of (unit -> Type)

val internal Delay: ('a -> 'b) -> 'a -> Type 

val internal Take: value -> 'a