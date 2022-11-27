module Position

type Position

val Position: int * int * int -> Position

val start: unit -> Position

val move: Position -> int -> unit

val next: Position -> unit

val newline: Position -> unit

val Copy: Position -> Position