module Result

type ('s,'e) Result with
static member mapWithError f ret =
    match ret with
    | Error e -> Error e
    | Ok r -> f r

static member map2 f ret1 ret2 =
    Result.map f ret1
    |> Result.map (fun f -> f ret2)

static member mapWithError2 f ret1 ret2 =
    match Result.map2 f ret1 ret2 with
    | Error msg -> Error msg
    | Ok ok -> ok
    
