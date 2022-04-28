module Interfaces
open Position


type Pos =
    abstract member Pos: Position

let PosOf (pos: Pos) = pos.Pos

type 'a Value =
    abstract member Cast : Result<'a,string>

let (!!) (value : _ Value) = value.Cast


let ValueOf item =
    match !!item with
    | Ok value -> value
    | Error msg ->
        printfn "%s" msg
        exit -1 // terminate the program
    

type 't Type =
    abstract member typeof: 't

let TypeOf (item : _ Type) = item.typeof
    