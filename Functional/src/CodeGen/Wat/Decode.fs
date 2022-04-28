module Decode

open CodeGen.Parser
open System.Text

open AbstractSyntax.Wasm
open Wasm
open Types
open Values
(*

    The Decoder for the Wat format is made up of
    parser combinators see Parser.fs

*)

let utf8 (txt: string) =
    Encoding.UTF8.GetBytes txt
    

let (~&) keyword =
    utf8 keyword
    |> Array.map Expect
    |> Sequence (fun pat p -> (string << char) p + pat) "" 



let inline leftrec pat =
    let rec loop input =
        (pat <&> (Parser loop <|> pat))
        |> Map (fun (x, y) -> (x + y)) 
        |> fun pat -> Run pat input

    Parser loop

let inline (<*>) star next =
    let rec loop input =
        match Run star input with
        | Error _ -> Run next input
        | Ok(item, rest) -> 
            match Run next rest with
            | Error _ -> Run next input
            | Ok(item', rest') ->
                match loop rest with
                | Error _ -> Ok(item + item', rest')
                | Ok(item'', rest'') -> Ok(item + item'', rest'')
    Parser loop

let (<?>) maybe next = 
    (maybe <|> Bind "") <&> next
    |> Map (fun (a, b) -> a + b)


let inline (<+>) plus next = 
    plus <&> (plus <*> next)
    |> Map (fun (plus, rest) -> plus + rest)




let Global = &"global"
let Local = &"local"
let Param = &"param"
let Dot = &"."

let trailing = 0b10000000uy
let mask2    = 0b11000000uy
let mask3    = 0b11100000uy
let mask4    = 0b11110000uy

// ut8 parser
let Char =
    let onebyte = 
        Predict (fun b -> b < 0x80uy)
        |> Map (string << char)

    let twobytes = 
        Predict (fun b -> b &&& mask2 <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy)
        |> Map (fun (fb, sb) -> (string << char) (fb ^^^ mask2) + (string << char) (sb ^^^ trailing))

    let threebytes =
        Predict (fun b -> b &&& mask3 <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy) 
        |> Map (fun ((fb, sb), tb) -> (string << char) (fb ^^^ mask3 ) + (string << char) (sb ^^^ trailing) + (string << char) (tb ^^^ trailing))

    let fourbytes =
        Predict (fun b -> b &&& mask3 <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy) <&> Predict (fun b -> b &&& trailing <> 0uy) 
        |> Map (fun (((b1, b2), b3), b4) -> (string << char) (b1 ^^^ mask4 ) + (string << char) (b2 ^^^ trailing) + (string << char) (b3 ^^^ trailing) + (string << char) (b4 ^^^ trailing))

    fun input ->
        Run (onebyte <|> twobytes <|> threebytes <|> fourbytes) input
        |> Result.mapError (fun c -> $"The character {c} is not part of UTF-8")
    |> Parser


let Par body = 
    &"(" <&> body <&> &")" 
    |> Map (fun ((_, body), _) -> body)

let Scope body =
    &"{" <&> body <&> &"}" 
    |> Map (fun ((_, body), _) -> body)


let I32 = &"i32"
let I64 = &"i64"
let F32 = &"f32"
let F64 = &"f64"

let NumType = 
    Map (fun _ -> I32_t) I32 
    <|> Map (fun _ -> I64_t) I64 
    <|> Map (fun _ -> F32_t) F32
    <|> Map (fun _ -> F64_t) F64


let FuncRef = &"funcref"
let ExternRef = &"externRef"

let RefType = Map (fun _ -> refType.FuncRef) FuncRef  <|> Map (fun _ -> refType.ExternRef)ExternRef

let ValueType =
    Map (fun nt -> valueType.NumType nt) NumType
    <|> Map (fun rt -> valueType.RefType rt) RefType


let plus = 
    &"+" <|> Bind ""
    |> Map (fun _ -> "+")

let sign =
    Map (fun _ -> "-") &"-" <|> plus

let digit = 
    Choose [ for d in '0' .. '9' ->  &(string d) ]
    


let hexdigit = 
    let h =
        Choose [for h in ['a' .. 'f'] -> &(string h) ]
        
    let H = 
        Choose [for h in ['A' .. 'F'] -> &(string h) ]

    digit <|> h <|> H



