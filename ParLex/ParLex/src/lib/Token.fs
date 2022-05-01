module Token

open Position
open TypeAcrobatics




[<Struct>]
type 'Type Token =
    val tp : 'Type
    val value : token
    val pos : Position
    new(tp, data, pos) = { tp=tp; value=data; pos=pos}


let inline TypeOf (token : _ Token) = token.tp

let inline ValueOf (token : _ Token) = 
    let t = Take token.value
    id t 

let inline PosOf (token : _ Token) = token.pos
