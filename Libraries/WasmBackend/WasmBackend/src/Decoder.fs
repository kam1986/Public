module Decoder


type Result<'success,'error> with

    static member returnOnError f ret =
        match ret with
        | Error _ -> f
        | ok -> ok

   


[<Struct>]
type Decoder<'input, 'output> = Decoder of ('input -> Result<'output * 'input, string> ) 


let Run (Decoder d) = d

let (!) item =
    fun input -> Ok(item, input)
    |> Decoder

let DecError msg =
    Error $"Decoding Error:\n {msg}"

let Expect e =
    fun input ->
        match input with
        | [] -> DecError "End Of File"
        | b :: rest when b = e -> Ok(e, rest)
        | b :: _ -> DecError $"Expected {e} but got {b}"
    |> Decoder

let (<|>) dec1 dec2 =
    fun input ->
        Run dec1 input 
        |> Result.returnOnError (Run dec2 input)
    |> Decoder

let Choose decs = List.reduce (<|>) decs

let (<&>) dec1 dec2 =
    fun input ->
        match Run dec1 input with
        | Error msg -> Error msg
        | Ok(ret1, rest) -> 
            match Run dec2 rest with
            | Error msg -> Error msg
            | Ok(ret2, rest) -> Ok((ret1, ret2), rest)
    |> Decoder    


let Map f dec =
    fun input -> 
        Run dec input
        |> Result.map (fun (ret, rest) -> f ret, rest)
    |> Decoder
            

let (=>) fAsDec a =
    fAsDec <&> a
    |> Map (fun (f, a) -> f a)

let Lift2 func param1 param2 =
    !func => param1 => param2

let (!!) func = Lift2 func

let Collect collector acc decoder =
    List.foldBack (fun a acc -> !!collector a acc) acc decoder