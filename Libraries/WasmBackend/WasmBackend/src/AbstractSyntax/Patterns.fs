module Patterns

open Position

type Pattern<'input,'output> = internal Pattern of ('input -> Result<'output * 'input, string>)


let Run (Pattern p) = p

let Bind item =
    fun input -> Ok(item, input)
    |> Pattern

let Expect item =
    fun (input, pos) ->
        match Seq.tryHead input with
        | None -> Error $"End of File at {pos}"
        // using item instead it should be able to optimize update to one of the branches
        // since it is known at compiletime
        | Some b when b = item -> Ok(item, (Seq.tail input, Update pos item)) 
        | Some b -> Error $"Expected {char item} but found {char b}"
    |> Pattern

let GetPos p =
    fun ((_, pos) as input) ->
        match Run p input with
        | Error msg -> Error $"{msg} at {pos}"
        | Ok(ret, rest) -> Ok((ret,pos), rest)
    |> Pattern

let Map f p =
    fun input ->
        Run p input
        |> Result.map (fun (r, rest) -> f r, rest)
    |> Pattern

let MapError e p =
    fun input ->
        Run p input
        |> Result.mapError e
    |> Pattern

let ( <|> ) p1 p2 =
    fun input ->
        match Run p1 input with
        | Ok ret -> Ok ret
        | Error _ -> Run p2 input
    |> Pattern

let Choose plst = Seq.reduce (<|>) plst

let ( <&> ) p1 p2 =
    fun input ->
        match Run p1 input with
        | Error msg -> Error msg
        | Ok(r1, rest) -> 
            match Run p2 rest with
            | Error msg -> Error msg
            | Ok(r2, rest) -> Ok((r1, r2), rest)
    |> Pattern


let ( => ) f p = Map (fun (f, r) -> f r) (f <&> p) 



let Lift2 f p1 p2 = Bind f => p1 => p2
    
let Collect plst =
    let cons x xs = x :: xs
    let collect = Lift2 cons
    Seq.foldBack (fun p acc -> collect p acc) plst (Bind [])
    |> Map (fun lst -> System.Text.Encoding.UTF8.GetString(List.toArray lst))


    
    