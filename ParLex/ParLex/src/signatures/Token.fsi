module Token

open Position
open TypeAcrobatics

type 'Type Token

val Token: 'Type * value * Position

val TypeOf: 'Type Token -> 'Type
val ValuOf: 'Type Token -> 'a
val PosOf: 'Type Token -> Position