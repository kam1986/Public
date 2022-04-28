module Helpers


type Op<'int,'float> = Integer of 'int | Float of 'float

type Result<'succes,'error> with
    static member Collaps ret =
        match ret with
        | Error msg -> Error msg
        | Ok ret -> ret

    static member CollapsError ret =
        match ret with
        | Error ret -> ret
        | Ok ret-> Ok ret

    static member Swap ret =
        match ret with
        | Error ret -> Ok ret
        | Ok ret -> Error ret

    static member map2 f ret1 ret2 =
        match ret1, ret2 with
        | Error msg, _| _, Error msg -> Error msg
        | Ok ret1, Ok ret2 -> Ok(f ret1 ret2)

    static member mapError2 f err1 err2 =
        match err1, err2 with
        | Error err1, Error err2 -> Error (f err1 err2)
        | Error err, _ | _, Error err -> Error err
        | _, _ -> err1

    static member mapWithError f ret =
        match ret with
        | Error e -> Error e
        | Ok ret -> f ret

    member r.isError = match r with Error _ -> true | Ok _ -> false
    
    member r.isResult = match r with Error _ -> false | Ok _ -> true

    static member IsError (r: Result<_,_>) = r.isError 

    static member IsResult (r: Result<_,_>) = r.isResult

    member r.Success = 
        match r with
        | Ok ret -> ret
        | _ -> Failure "" |> raise 

type Option<'item> with
    static member Collaps item =
        match item with
        | None -> None
        | Some item -> item


let (~&) item = seq[item]

let (+>) item sq = Result.map (fun sq -> seq[yield item; yield! sq]) sq

let (<+) sq item = Result.map (fun sq -> seq[yield! sq; yield item]) sq

let (<+>) sq1 sq2 = 
    match sq1, sq2 with
    | Error msg1, Error msg2 -> Error $"{msg1}\n{msg2}"
    | Error msg, _ | _, Error msg -> Error msg
    | Ok sq1, Ok sq2 -> Ok(seq[yield! sq1; yield! sq2])

let (++) item1 item2 = Ok (seq[yield item1; yield item2])


let curry f a b = f (a,b)
let uncurry f (a,b) = f a b
let swap f a b = curry (fun (a,b) -> (b,a)) a b |> uncurry f

let ToArray (s: _ seq) = 
    try 
         s :?> _ array
    with  _ -> Seq.toArray s



    





