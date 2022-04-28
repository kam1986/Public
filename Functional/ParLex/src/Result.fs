module Return



let debugReturn ret =
    let test ret =
        match ret with
        | Ok _ -> true
        | Error msg -> 
            printfn "%s" msg
            false
    assert test ret
    match ret with
    | Ok ret -> ret
    | Error msg ->
        printfn "%s" msg
        exit -1

let Return ret =
    match ret with
    | Ok ret -> ret
    | Error msg ->
        printfn "%s" msg
        exit -1

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