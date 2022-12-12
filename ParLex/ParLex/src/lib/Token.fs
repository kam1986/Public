module Token

open Position
open TypeAcrobatics




[<Struct>]
type 'Type Token =
    val internal tp : 'Type
    val internal value : token
    val internal pos : Position
    new(tp, data, pos) = { tp=tp; value=data; pos=pos}

with
    static member map f (token: _ Token) = Token(f token.tp, token.value, token.pos)

let TypeOf (token : _ Token) = token.tp

let ValueOf (token : _ Token) = 
    let t = Take token.value
    id t 

let PosOf (token : _ Token) = token.pos
