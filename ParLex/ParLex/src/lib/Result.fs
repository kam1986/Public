module Return

exception ResultError of string


let debugReturn ret =
    match ret with
    | Ok ret -> ret
    | Error msg ->
        ResultError msg
        |> raise

let Return ret =
    match ret with
    | Ok ret -> ret
    | Error msg ->
        printfn "%s" msg
        ResultError msg
        |> raise

let map f ret =
    match ret with
    | Ok a -> Ok (f a)
    | Error msg -> Error msg

let ErrorMap f ret =
    match ret with
    | Ok a -> Ok a
    | Error msg -> Error (f msg) 

let ErrorHandler f ret =
    match ret with
    | Ok a -> Ok a
    | Error msg -> f msg 